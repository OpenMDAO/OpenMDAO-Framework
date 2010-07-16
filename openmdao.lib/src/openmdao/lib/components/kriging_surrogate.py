from math import log,e,sqrt
from numpy import array,zeros,dot,ones,arange,eye,abs,append
from numpy.linalg import det,pinv,linalg,lstsq
from scipy.linalg import cho_factor,cho_solve,lu_factor,lu_solve,triu,LinAlgError
from scipy.optimize import fmin, anneal, brute

from enthought.traits.api import HasTraits, implements

from openmdao.main.interfaces import ISurrogate
from openmdao.main.uncertain_distributions import NormalDistribution


class KrigingSurrogate(HasTraits): 
    implements(ISurrogate)    
    
    def __init__(self,X=None,Y=None):
        super(KrigingSurrogate, self).__init__() # must call HasTraits init to set up Traits stuff

        self.m = None #number of independent
        self.n = None #number of training points
        self.thetas = None
        
        self.R = None
        self.mu = None
        self.sig2 = None
        self.log_likelihood = None
        
        self.X = X
        self.Y = Y

        if X is not None and Y is not None: 
            self.train(X,Y)
            
    def get_uncertain_value(self,value): 
        """returns a NormalDistribution centered around the value, with a standard deviation of 0"""
        return NormalDistribution(value,0)

    def predict(self,new_x):
        """calculates a predicted value of the response, based on the current 
        trained model for the supplied list of inputs
        """
        if self.m == None: #untrained surrogate
            raise RuntimeError("KrigingSurrogate has not been trained, so no prediction can be made")
        r = zeros(self.n)
        X,Y = self.X,self.Y
        thetas = 10**self.thetas
        for i in range(self.n):
            r[i] = e**(-sum(thetas*(X[i]-new_x)**2))
        one = ones(self.n)

        #-----LSTSQ-------
        f = self.mu+dot(r,lstsq(self.R,Y-dot(one,self.mu))[0])
        term1 = dot(r,lstsq(self.R,r)[0])
        term2 = (1-dot(one,lstsq(self.R,r)[0]))**2/dot(one,lstsq(self.R,one)[0])
        #---LU or CHOLESKY DECOMPOSTION ---
        #R_fact = self.R_fact
        #f = self.mu+dot(r,self.myfun(R_fact,Y-dot(one,self.mu)))
        #term1 = dot(r,self.myfun(R_fact,r))
        #term2 = (1-dot(one,self.myfun(R_fact,r)))**2/dot(one,self.myfun(R_fact,one))

        MSE = self.sig2*(1-term1+term2)
        RMSE = sqrt(abs(MSE))
        
        return NormalDistribution(f,RMSE)

    def train(self,X,Y):
        """train the surrogate model with the given set of inputs and outputs"""
        
        #TODO: Check if one training point will work... if not raise error
        self.X = X
        self.Y = Y
        self.m = len([0])
        self.n = len(X)
        thetas = zeros(self.m)
        def _calcll(thetas):
            self.thetas = thetas
            self._calculate_log_likelihood()
            return -self.log_likelihood
        self.thetas = fmin(_calcll, thetas, disp=False, ftol = 0.0001)
        self._calculate_log_likelihood()
        
    def _calculate_log_likelihood(self):
        #if self.m == None:
        #    Give error message
        R = zeros((self.n,self.n))
        X,Y = self.X,self.Y
        thetas = 10**self.thetas
        for i in range(self.n):
            for j in arange(i+1,self.n):
                R[i,j] = e**(-sum(thetas*(X[i]-X[j])**2)) #weighted distance formula
        R = R+R.T+eye(self.n)
        self.R = R
        one = ones(self.n)
        try:
            self.R_fact = cho_factor(R)
            self.myfun = cho_solve
            self.mu = dot(one,self.myfun(self.R_fact,Y))/dot(one,self.myfun(self.R_fact,one))
            self.sig2 = dot(Y-dot(one,self.mu),self.myfun(self.R_fact,(Y-dot(one,self.mu))))/self.n
            self.log_likelihood = -self.n/2*log(self.sig2)-1./2.*log(abs(det(self.R)))-sum(self.thetas)-sum(abs(self.thetas))
        except linalg.LinAlgError:
            #---LU DECOMPOSITION---
            #self.R_fact = lu_factor(R)
            #self.myfun = lu_solve
            #self.mu = dot(one,self.myfun(self.R_fact,Y))/dot(one,self.myfun(self.R_fact,one))
            #self.sig2 = dot(Y-dot(one,self.mu),self.myfun(self.R_fact,(Y-dot(one,self.mu))))/self.n
            #------LSTSQ---------
            self.mu = dot(one,lstsq(self.R,Y)[0])/dot(one,lstsq(self.R,one)[0])
            self.sig2 = dot(Y-dot(one,self.mu),lstsq(self.R,(Y-dot(one,self.mu)))[0])/self.n
            self.log_likelihood = -self.n/2*log(self.sig2)-1./2.*log(abs(det(self.R)+1e-16))-sum(self.thetas)
                
if __name__ == "__main__": 
    from matplotlib import pyplot as py
    X = array([[0.05], [.25], [0.61], [0.95]])
    Y1 = array([0.738513784857542,-0.210367746201974,-0.489015457891476,12.3033138316612])
    Y2 = array([-9.1307431075712291, -7.6051838731009873, -4.1445077289457384, 5.6516569158305785])
    rse1 = KrigingSurrogate(X,Y1)
    rse2 = KrigingSurrogate(X,Y2)

    x = arange(0.05,0.95,0.1)
    f_1,f_2,err_1,err_2 = [],[],[],[]

    for case in x:
        y1 = rse1.predict(case)
        y2 = rse2.predict(case)
        f_1.append(y1.mu)
        err_1.append(y1.sigma)
        f_2.append(y2.mu)
        err_2.append(y2.sigma)

    py.figure()
    py.subplot(211)
    py.plot(x,f_1)
    py.plot(x,f_2)
    py.subplot(212)
    py.plot(x,err_1)
    py.plot(x,err_2)

    py.show()