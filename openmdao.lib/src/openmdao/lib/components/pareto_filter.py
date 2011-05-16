""" Pareto Filter -- finds non-dominated cases. """

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Instance, List, ListStr
from openmdao.lib.casehandlers.api import CaseSet, caseiter_to_caseset

from openmdao.main.component import Component
from openmdao.main.pluginsock import Socket
from openmdao.main.interfaces import ICaseIterator
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator

class ParetoFilter(Component):
    """Takes a set of cases and filters out the subset of cases which are
    pareto optimal. Assumes that smaller values for model responses are
    better, so all problems must be posed as minimization problems.
    """
    
    # pylint: disable-msg=E1101
    criteria = ListStr([], iotype="in",
                       desc="List of outputs from the case to consider for "
                            "filtering. Note that only case outputs are allowed as "
                            "criteria.")
    
    #case_set = Instance(ICaseIterator, iotype="in",
    #                    desc="CaseIterator with the cases to be filtered to "
    #                         "Find the pareto optimal subset.")
                             
    case_sets = List(Socket(ICaseIterator), value=[], iotype="in",
                     desc="CaseSet with the cases to be filtered to "
                     "Find the pareto optimal subset.")
    
    pareto_set = Socket(CaseSet, iotype="out", 
                        desc="Resulting collection of pareto optimal cases.",copy="shallow")
    dominated_set = Socket(CaseSet, iotype="out",
                           desc="Resulting collection of dominated cases.",copy="shallow")
    
    def _is_dominated(self, y1, y2):
        """Tests to see if the point y1 is dominated by the point y2. 
        True if y1 is dominated by y2, False otherwise.
        """
        if y1 == y2:
            return False
        for a,b in zip(y1, y2): 
            if a<b: return False

        return True
    
    def execute(self):
        """Finds and removes pareto optimal points in the given case set.
        Returns a list of pareto optimal points. Smaller is better for all
        criteria.
        """
        #convert stuff to caseSets if they are not 
        case_sets = []
        for ci in self.case_sets: 
            if not isinstance(ci,CaseSet): 
                case_sets.append(caseiter_to_caseset(ci))
            else: 
                case_sets.append(ci)
        
        y_list = []
        if len(case_sets) > 1: 
            case_set = case_sets[0].union(*case_sets[1:])
        else: 
            case_set = case_sets[0]
        criteria_count = len(self.criteria)
        
        try: 
            # need to transpose the list of outputs
            y_list = zip(*[case_set[crit] for crit in self.criteria]) 
        except KeyError: 
            self.raise_exception('no cases provided had all of the outputs '
                 'matching the provided criteria, %s'%self.criteria, ValueError)
            
        y_temp = list(y_list)
        
        self.dominated_set = CaseSet()
        self.pareto_set = CaseSet() #TODO: need a way to copy casesets

        for point1, case in zip(y_list, iter(case_set)):
            dominated = False
            for point2 in y_temp:
                if self._is_dominated(point1, point2):
                    self.dominated_set.record(case)
                    y_temp.remove(point1)
                    dominated = True
                    break
            if not dominated: 
                self.pareto_set.record(case)
     
if __name__ == "__main__": # pragma: no cover  
    
    # pylint: disable-msg=C0103, E1101
    
    from matplotlib import pyplot as py
    from mpl_toolkits.mplot3d import Axes3D
    from numpy import random
    random.seed(10)
    
    from openmdao.main.case import Case
    pf = ParetoFilter()
    
    # 2D PARETO FILTERING EXAMPLE
    n = 1000
    x = random.uniform(-1, 0, n)
    y = -(1-x**2)**0.5*random.random(n)
    cases = CaseSet()
    for x_0, y_0 in zip(x, y):
        cases.record(Case(inputs=[("x", x_0), ("y", y_0)]))

    pf.case_sets = [cases]
    pf.criteria = ['x', 'y']
    pf.execute()
    
    x_p, y_p = pf.pareto_set['x'],pf.pareto_set['y']
    x_dom, y_dom = pf.dominated_set['x'],pf.dominated_set['y']
    
    py.figure()
    py.scatter(x, y, s=5)
    py.scatter(x_dom, y_dom, c='', edgecolor='b', s=80)
    py.scatter(x_p, y_p, c='', edgecolors='r', s=80)
    
    #3D PARETO FILTERING EXAMPLE
    n = 1000
    x = random.uniform(-1, 0, n)
    y = -(1-x**2)**0.5*random.random(n)
    z = -(1-x**2-y**2)**0.5*random.random(n)
    doe = zip(x, y, z)
    
    pf.criteria = ['x', 'y', 'z']
    
    
    cases = CaseSet()
    for x_0, y_0, z_0 in zip(x, y, z):
        cases.record(Case(inputs=[("x", x_0),
                                   ("y", y_0),
                                   ("z", z_0)]))
    
    pf.case_sets = [cases]
    pf.execute()
   
    x_p, y_p, z_p = pf.pareto_set['x'], pf.pareto_set['y'], pf.pareto_set['z']
    x_dom, y_dom, z_dom = pf.dominated_set['x'], pf.dominated_set['y'], pf.dominated_set['z']
    fig1 = py.figure()
    a1 = Axes3D(fig1)
    a1.scatter(x_dom, y_dom, z_dom, c='b')
    a1.scatter(x_p, y_p, z_p, c='r', edgecolor='r')
    
    py.show()
