from time import time

try:
    from numpy import exp, abs, pi, array,isnan,sum,sqrt,argsort, min
    from scipy.special import erf
    from scipy.integrate import dblquad
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))

from openmdao.lib.datatypes.api import Str, ListStr, Enum, \
     Float, Array, Event, List

from openmdao.main.component import Component

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.util.decorators import stub_if_missing_deps

@stub_if_missing_deps('numpy', 'scipy')
class Pareto_Min_Dist(Component):
    """Computes the probability that any given point from the primary concept 
    will interesect the pareto frontiers of some other concepts.
    """ 
    pareto = List([], iotype="in",
                    desc="List of CaseIterators containing competing local Pareto points")
                    
    criteria = ListStr(iotype="in",dtype="str",
                       desc="Names of responses to maximize expected improvement around. "
                            "Must be NormalDistribution type.")
    
    predicted_values = Array(iotype="in",dtype=NormalDistribution,
                             desc="CaseIterator which contains a NormalDistribution "
                                  "for each response at a location where you wish to "
                                  "calculate EI.")
    
    dist = Float(0.0, iotype="out", 
                 desc="minimum distance from a point to other pareto set ")
    
    reset_pareto = Event()
    
    def __init__(self,*args,**kwargs):
        super(Pareto_Min_Dist, self).__init__(*args, **kwargs)
        self.y_star_other = None
        
    def _reset_pareto_fired(self):
        self.y_star_other = None
    
    def get_pareto(self):
        y_star_other = []

        c = []
             
        for single_case_list in self.pareto:
            for case in single_case_list:
                for objective in case.outputs:
                    for crit in self.criteria:
                        if crit in objective[0]:
                            #TODO: criteria needs at least two things matching
                            #objective names in CaseIterator outputs, error otherwise
                            c.append(objective[2])
                if c != [] :
                    y_star_other.append(c)
                c = []
       
        return y_star_other
        
        
    def _calc_min_dist(self,p,y_star_other):
        """Computes the minimum distance from a candidate point 
        to other_pareto.
        """
        
        dists = []
        
        for y in y_star_other:
            d = sqrt(sum([(A-B)**2 for A,B in zip(p,y)]))
            dists.append(d)

        return min(dists)
        
    def execute(self):
        mu = [objective.mu for objective in self.predicted_values]

        if self.y_star_other == None:
            self.y_star_other = self.get_pareto()
        
        self.dist = self._calc_min_dist(mu,self.y_star_other)
        