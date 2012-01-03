"""Expected Improvement calculation for one or more objectives.""" 

import logging

try:
    from numpy import exp, abs, pi, array,isnan, diag, random
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
_check=['numpy']
try:
    from math import erf
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
    try:
        from scipy.special import erf
    except ImportError as err:
        logging.warn("In %s: %r" % (__file__, err))
        _check.append('scipy')

from openmdao.lib.datatypes.api import Slot, Str, ListStr, Enum, \
     Float, Array,Event, Int

from openmdao.main.component import Component
from openmdao.util.decorators import stub_if_missing_deps

from openmdao.lib.casehandlers.api import CaseSet
from openmdao.main.uncertain_distributions import NormalDistribution

@stub_if_missing_deps(*_check)
class MultiObjExpectedImprovement(Component):
    best_cases = Slot(CaseSet, iotype="in",
                    desc="CaseIterator which contains only Pareto optimal cases \
                    according to criteria")
    
    criteria = Array(iotype="in",
                    desc="Names of responses to maximize expected improvement around. \
                    Must be NormalDistribution type.")
    
    predicted_values = Array(iotype="in",dtype=NormalDistribution,
                        desc="CaseIterator which contains NormalDistributions for each \
                        response at a location where you wish to calculate EI.")
    
    n = Int(1000,iotype="in",desc="number of Monte Carlo Samples with \
                        which to calculate probability of improvement")
    
    calc_switch = Enum("PI",["PI","EI"],iotype="in",desc="switch to use either \
                        probability (PI) or expected (EI) improvement")
    
    PI = Float(0.0, iotype="out", desc="The probability of improvement of the next_case")
    
    EI = Float(0.0, iotype="out", desc="The expected improvement of the next_case")

    reset_y_star = Event()
    
    def __init__(self, *args, **kwargs):
        super(MultiObjExpectedImprovement, self).__init__(*args, **kwargs)
        self.y_star = None
        
    def _reset_y_star_fired(self):
        self.y_star = None
    
    def get_y_star(self):
        criteria_count = len(self.criteria)
        
        flat_crit= self.criteria.ravel()

        try:
            y_star = zip(*[self.best_cases[crit] for crit in self.criteria])
        except KeyError:
            self.raise_exception('no cases in the provided case_set had output '
                 'matching the provided criteria, %s'%self.criteria, ValueError)
        
        #sort list on first objective
        y_star = array(y_star)[array([i[0] for i in y_star]).argsort()]
        return y_star
        
    def _2obj_PI(self,mu,sigma):
        """Calculates the multi-objective probability of improvement
        for a new point with two responses. Takes as input a 
        pareto frontier, mean and sigma of new point."""
        
        y_star = self.y_star

        PI1 = (0.5+0.5*erf((1/(2**0.5))*((y_star[0][0]-mu[0])/sigma[0])))
        PI3 = (1-(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0]))))\
        *(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))
     
        PI2 = 0
        if len(y_star)>1:
            for i in range(len(y_star)-1):
                PI2=PI2+((0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))\
                -(0.5+0.5*erf((1/(2**0.5))*((y_star[i][0]-mu[0])/sigma[0]))))\
                *(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))
        mcpi = PI1+PI2+PI3
        return mcpi
    
    def _2obj_EI(self,mu,sigma):
        """Calculates the multi-criteria expected improvement
        for a new point with two responses. Takes as input a 
        pareto frontier, mean and sigma of new point."""
        
        y_star = self.y_star
        ybar11 = mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[0][0]-mu[0])/sigma[0])))\
        -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[0][0]-mu[0])**2/sigma[0]**2))
        ybar13 = (mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0])))\
        -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[-1][0]-mu[0])**2/sigma[0]**2)))\
        *(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))
        
        ybar12 = 0
        if len(y_star)>1:
            for i in range(len(y_star)-1):
                ybar12 = ybar12+((mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))\
                -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i+1][0]-mu[0])**2/sigma[0]**2)))\
                -(mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i][0]-mu[0])/sigma[0])))\
                -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i][0]-mu[0])**2/sigma[0]**2))))\
                *(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))

        ybar1 = (ybar11+ybar12+ybar13)/self.PI
        
        ybar21 = mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[0][1]-mu[1])/sigma[1])))\
        -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[0][1]-mu[1])**2/sigma[1]**2))
        ybar23 = (mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))\
        -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[-1][1]-mu[1])**2/sigma[1]**2)))\
        *(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0])))

        ybar22 = 0
        if len(y_star)>1:
            for i in range(len(y_star)-1):
                ybar22 = ybar22+((mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))\
                -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i+1][1]-mu[1])**2/sigma[1]**2)))\
                -(mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i][1]-mu[1])/sigma[1])))\
                -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i][1]-mu[1])**2/sigma[1]**2))))\
                *(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))
        
        ybar2 = (ybar21+ybar22+ybar23)/self.PI
        dists = [((ybar1-point[0])**2+(ybar2-point[1])**2)**0.5 for point in y_star]
        mcei = self.PI*min(dists)
        if isnan(mcei):
            mcei = 0
        return mcei
 
    def _dom(self,a,b):
        """determines if a completely dominates b
       returns True is if does
    """
        comp = [c1<c2 for c1,c2 in zip(a,b)]
        if sum(comp)==len(self.criteria):
            return True        
        return False
    
    def _nobj_PI(self,mu,sigma):
        cov = diag(array(sigma)**2)
        rands = random.multivariate_normal(mu,cov,self.n)
        num = 0 #number of cases that dominate the current Pareto set

        for random_sample in rands:
            for par_point in self.y_star:
                #par_point = [p[2] for p in par_point.outputs]
                if self._dom(par_point,random_sample):
                    num = num+1
                    break
        pi = (self.n-num)/float(self.n)
        return pi
        
    def execute(self): 
        """ Calculates the expected improvement or 
        probability of improvement of a candidate 
        point given by a normal distribution.
        """
        mu = [objective.mu for objective in self.predicted_values]
        sig = [objective.sigma for objective in self.predicted_values]
        
        if self.y_star == None:
            self.y_star = self.get_y_star()

        n_objs = len(self.criteria)

        if n_objs==2:
            """biobjective optimization"""
            self.PI = self._2obj_PI(mu,sig)
            if self.calc_switch == 'EI':
                """execute EI calculations"""
                self.EI = self._2obj_EI(mu,sig)
        if n_objs>2: 
            """n objective optimization"""
            self.PI = self._nobj_PI(mu,sig)
            if self.calc_switch == 'EI':
                """execute EI calculations"""
                self.raise_exception("EI calculations not supported"
                                        " for more than 2 objectives", ValueError)
