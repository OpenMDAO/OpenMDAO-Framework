from enthought.traits.api import Instance

from openmdao.main.component import Component
from openmdao.lib.traits.array import Array
from openmdao.main.interfaces import ICaseIterator
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator



class ParetoFilter(Component): 
    """takes a set of cases and filters out the subset of cases which are pareto optimal. Assumes that smaller values for 
       model responses are better, so all problems must be posed as minimization problems""" 
    
    case_set = Instance(ICaseIterator,iotype="in",desc="CaseIterator with the cases to be filtered to find the pareto optimal subset")
    
    pareto_set = Instance(ICaseIterator,iotpye="out",desc="resulting collection of pareto optimal cases")
    dominated_set = Instance(ICaseIterator,iotype="out",desc="resulting collection of dominated cases")
                      
    def _is_dominated(self,y1,y2):
        """tests to see if the point y1 is dominated by the point y2. 
        True if y1 is dominated by y2, False otherwise.
        """
        if y1==y2: return False
        if any([a<b for a,b in zip(y1,y2)]): return False
        return True
    
    def execute(self):
        """Finds and removes pareto optimal points in the given case set. Returns 
        list of pareto optimal points. Smaller is better for all criteria.
        """
        
        y_list = []
        cases = [case for case in self.case_set]
        
        for case in cases:
            y_list.append([o[2] for o in case.outputs])
        y_temp = list(y_list)
        
        dominated_set =[]
        pareto_set = list(cases)
        for point1,case in zip(y_list,cases):
            for point2 in y_temp:
                if self._is_dominated(point1,point2):
                    dominated_set.append(case)
                    y_temp.remove(point1)
                    pareto_set.remove(case)
                    break
        
        self.pareto_set = ListCaseIterator(pareto_set)
        self.dominated_set = ListCaseIterator(dominated_set)
    