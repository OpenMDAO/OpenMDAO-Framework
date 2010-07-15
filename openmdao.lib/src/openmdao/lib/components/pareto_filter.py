from openmdao.main.component import Component
from openmdao.lib.traits.array import Array



class ParetoFilter(Component): 
    """takes a set of cases and filters out the subset of cases which are pareto optimal. Assumes that smaller values for 
       model responses are better, so all problems must be posed as minimization problems""" 

    case_set = Array([],iotype="in",desc="set of cases to be filtered to find the pareto optimal subset")
    pareto_set = Array([],iotpye="out",desc="resulting collection of pareto optimal cases")
    dominated_set = Array([],iotype="out",desc="resulting collection of dominated cases")
                      
    def _is_dominated(self,y1,y2):
        """tests to see if the point y1 is dominated by the point y2. True if y1 is dominated by y2, False otherwise"""
        if y1==y2: return False
        if any([a<b for a,b in zip(y1,y2)]): return False
        return True
    
    
    def execute(self):
        """Finds and removes pareto optimal points in the given case set. Returns 
        list of pareto optimal points. Smaller is better for all criteria."""
        
        y_list = []
        for case in self.case_set:
            y_list.append([o[2] for o in case.outputs])
        y_temp = list(y_list)
        self.dominated_set =[]
        self.pareto_set = list(self.case_set)
        for point1,case in zip(y_list,self.case_set):
            for point2 in y_temp:
                if self._is_dominated(point1,point2):
                    self.dominated_set.append(case)
                    y_temp.remove(point1)
                    self.pareto_set.remove(case)
                    break
        
    
    
if __name__ == "__main__":
    from matplotlib import pyplot as py
    from mpl_toolkits.mplot3d import Axes3D
    from numpy import random
    
    from openmdao.main.case import Case
    pf = ParetoFilter()
    
    
    # 2D PARETO FILTERING EXAMPLE
    n = 1000
    x = random.uniform(-1,0,n)
    cases = []
    for x_0,y_0 in zip(x,y):
        cases.append(Case(outputs=[("test",0,x_0),("test",1,y_0)]))
    
    pf.case_set = cases
    pf.execute()
    
    x_p,y_p = zip(*pf.pareto_set)
    x_dom,y_dom = zip(*pf.dominated_set)
    
    py.figure()
    py.scatter(x,y,s=5)
    py.scatter(x_dom,y_dom,c='',edgecolor='b',s=80)
    py.scatter(x_p,y_p,c='',edgecolors='r',s=80)
    
    #3D PARETO FILTERING EXAMPLE
    n = 100
    x = random.uniform(-1,0,n)
    y = -(1-x**2)**0.5*random.random(n)
    z = -(1-x**2-y**2)**0.5*random.random(n)
    doe = zip(x,y,z)
    
    cases = []
    for x_0,y_0,z_0 in zip(x,y,z):
        cases.append(Case(outputs=[("test",0,x_0),("test",1,y_0),("test",1,z_0)]))
    
    pf.case_set = cases
    pf.execute()
    
    y_star3D,y_dom3D = pf.pareto_set,pf.dominated_set
    x_p,y_p,z_p = zip(*y_star3D)
    x_dom,y_dom,z_dom = zip(*y_dom3D)

    fig1 = py.figure()
    a1 = Axes3D(fig1)
    a1.scatter(x_dom,y_dom,z_dom,c='b')
    a1.scatter(x_p,y_p,z_p,c='r',edgecolor='r')
    
    py.show()