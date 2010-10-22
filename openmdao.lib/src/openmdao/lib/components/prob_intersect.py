"""Computes the probability that any given point from the primary concept 
will interesect the pareto frontiers of some other concpets""" 

from numpy import exp, abs, pi, array,isnan,sum,sqrt,argsort
from scipy.special import erf
from scipy.integrate import dblquad

from enthought.traits.api import Instance, Str, ListStr, Array

from openmdao.lib.datatypes.enum import Enum
from openmdao.lib.datatypes.float import Float

from openmdao.main.component import Component

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.uncertain_distributions import NormalDistribution

class ProbIntersect(Component):
    primary_pareto = Instance(ICaseIterator, iotype="in",
                    desc="CaseIterator which contains only Pareto optimal cases \
                    belonging to the same model as predicted_values")
    
    global_pareto = Instance(ICaseIterator, iotype="in",
                    desc="CaseIterator which contains all the points from the\
                    globa pareto frontier")
                    
    criteria = ListStr(iotype="in",dtype="str",
                    desc="Names of responses to maximize expected improvement around. \
                    Must be NormalDistribution type.")
    
    predicted_values = Array(iotype="in",
                        desc="CaseIterator which contains a NormalDistribution for each \
                        response at a location where you wish to calculate EI.")
    
    PInt = Float(0.0, iotype="out", desc="The probability that a candidate point \
                                        is close to Pareto intersection")

    def _calcDist(self,p1,y_star_other):
        """Computes the minimum distance from a point in 
        primary_pareto to other_pareto. Uses this information
        to set limits of integration for _calcInt
        """
        
        dists = []
        
        for y in y_star_other:
            d = sqrt(sum([(A-B)**2 for A,B in zip(p1,y)]))
            dists.append(d)
        closest = y_star_other[array(dists).argsort()][0]    
        
        min_dists = abs(p1-closest)
        
        return min_dists
        
    def _calcProbInt_single(self,mu,sigma,pareto_point,y_star_other):
        """Computes the double integral of the predicted
        value given its normal distribution parameters (mu,sigma)
        The integral is taken over a single member of primary_pareto
        and bounds are found using _calcDist.
        """
        pp = array(pareto_point)
        ys = array(y_star_other)
        xd,yd = self._calcDist(pp,ys)
        #print xd,yd
        ai = 1./xd
        bi = 1./yd
        f1_low = pp[0]-ai
        f1_hi  = pp[0]+ai
        f2_low = pp[1]-bi
        f2_hi  = pp[1]+bi

        mu1 = mu[0]
        sig1 = sigma[0]
        sig1 = 2
        
        mu2 = mu[1]
        sig2 = sigma[1]
        sig2 = 2
        
        def joint_pdf(f2,f1):
            return 1/(2*pi*sig1*sig2)*exp(-0.5*(((f1-mu1)**2/sig1**2)+((f2-sig2)**2/sig2**2)))
        
        integral = dblquad(joint_pdf, f1_low , f1_hi, lambda f1: f2_low, lambda f1: f2_hi)

        return integral[0]
        
    def _calcProbInt(self,y_star,y_star_other):
        """Computes the probability that a new point is
        close to a Pareto intersection. Makes sequential
        calls to _calcProbInt_single for each point in primary_pareto
        """
        mu = [objective.mu for objective in self.predicted_values]
        sig = [objective.sigma for objective in self.predicted_values]
        P = 0
        for single_pareto_point in y_star:
            P += self._calcProbInt_single(mu,sig,single_pareto_point,y_star_other)
        return P
        
    def execute(self):
        """ Calculates the probability that a new point
        is close to the intersection of Pareto frontiers.
        """
        
        #y_star is a 2D list of pareto points belonging
        #to the same model as predicted_values
        y_star = []
        y_star_other = []        

        c = []
        
        #find the pareto points which are in the global_pareto but not in the primary_pareto
        other_pareto = [case for case in self.global_pareto if case not in self.primary_pareto]
        
        for case in self.primary_pareto:

            for objective in case.outputs:
                for crit in self.criteria:
                    if crit in objective[0]:
                        #TODO: criteria needs at least two things matching
                        #objective names in CaseIterator outputs, error otherwise
                        c.append(objective[2])
            if c != [] :
                y_star.append(c)
            c = []


        for case in other_pareto:
            for objective in case.outputs:
                for crit in self.criteria:
                    if crit in objective[0]:
                        #TODO: criteria needs at least two things matching
                        #objective names in CaseIterator outputs, error otherwise
                        c.append(objective[2])
            if c != [] :
                y_star_other.append(c)
            c = []        
        
        self.PInt = self._calcProbInt(y_star,y_star_other)