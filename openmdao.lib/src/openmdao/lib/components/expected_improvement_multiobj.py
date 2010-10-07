"""Expected Improvement calculation for one or more objectives""" 

from numpy import exp, abs, pi, array,isnan
from scipy.special import erf

from enthought.traits.api import Instance, Str, ListStr, Array

from openmdao.lib.datatypes.enum import Enum
from openmdao.lib.datatypes.float import Float

from openmdao.main.component import Component

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.uncertain_distributions import NormalDistribution

class MultiObjExpectedImprovement(Component):
    best_cases = Instance(ICaseIterator, iotype="in",
                    desc="CaseIterator which contains only Pareto optimal cases \
                    according to criteria")
    
    criteria = ListStr(iotype="in",dtype="str",
                    desc="Names of responses to maximize expected improvement around. \
                    Must be NormalDistribution type.")
    
    predicted_values = Array(iotype="in",
                        desc="CaseIterator which contains NormalDistributions for each \
                        response at a location where you wish to calculate EI.")
    
    PI = Float(0.0, iotype="out", desc="The probability of improvement of the next_case")
    
    EI = Float(0.0, iotype="out", desc="The expected improvement of the next_case")

    def _multiPI(self,mu,sigma):
        """Calculates the multi-objective probability of improvement
        for a new point with two responses. Takes as input a 
        pareto frontier, mean and sigma of new point"""
        
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
    
    def _multiEI(self,mu,sigma):
        """Calculates the multi-criteria expected improvement
        for a new point with two responses. Takes as input a 
        pareto frontier, mean and sigma of new point"""
        
        y_star = self.y_star
        self.PI = self._multiPI(mu,sigma)
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

    def execute(self): 
        """ Calculates the expected improvement of
        the model at a given point.
        """
        
        #y_star is a 2D list of pareto points
        y_star = []
        c = []
        
        for case in self.best_cases:

            for objective in case.outputs :
                for crit in self.criteria: 
                    if crit in objective[0]:
                        #TODO: criteria needs at least two things matching
                        #objective names in CaseIterator outputs, error otherwise
                        c.append(objective[2])
            if c != [] :
                y_star.append(c)
            c = []
        mu = [objective.mu for objective in self.predicted_values]
        sig = [objective.sigma for objective in self.predicted_values]

        #self.y_star = y_star
        
        #sort list on first objective
        self.y_star = array(y_star)[array([i[0] for i in y_star]).argsort()]
        
        self.EI = self._multiEI(mu,sig)
        #print "ei: ", self.EI
        
        