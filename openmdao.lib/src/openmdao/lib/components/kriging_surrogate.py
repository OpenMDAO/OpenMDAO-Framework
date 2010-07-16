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
        R_fact = self.R_fact

        #f = self.mu+dot(r,self.myfun(R_fact,Y-dot(one,self.mu)))
        f = self.mu+dot(r,lstsq(self.R,Y-dot(one,self.mu))[0]) #-----LSTSQ-------

        #term1 = dot(r,lstsq(self.R,r)[0])
        #term2 = (1-dot(one,lstsq(self.R,r)[0]))**2/dot(one,lstsq(self.R,one)[0])
        term1 = dot(r,self.myfun(R_fact,r))
        term2 = (1-dot(one,self.myfun(R_fact,r)))**2/dot(one,self.myfun(R_fact,one))
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
            self.R_fact = lu_factor(R)
            self.myfun = lu_solve
            #self.mu = dot(one,self.myfun(self.R_fact,Y))/dot(one,self.myfun(self.R_fact,one))
            #self.sig2 = dot(Y-dot(one,self.mu),self.myfun(self.R_fact,(Y-dot(one,self.mu))))/self.n
            #------SECOND VERSION USING LSTSQ-------------
            self.mu = dot(one,lstsq(self.R,Y)[0])/dot(one,lstsq(self.R,one)[0])
            self.sig2 = dot(Y-dot(one,self.mu),lstsq(self.R,(Y-dot(one,self.mu)))[0])/self.n
            self.log_likelihood = -self.n/2*log(self.sig2)-1./2.*log(abs(det(self.R)+1e50))-sum(self.thetas)
                
if __name__ == "__main__": 
    from matplotlib import pyplot as py
    X = array([[0.05], [.25], [0.61], [0.95]])
    Y1 = array([0.738513784857542,-0.210367746201974,-0.489015457891476,12.3033138316612])
    Y2 = array([-9.1307431075712291, -7.6051838731009873, -4.1445077289457384, 5.6516569158305785])
    rse1 = KrigingSurrogate(X,Y1)
    rse2 = KrigingSurrogate(X,Y2)
    
    theta1 = rse1.thetas
    theta2 = rse2.thetas
    opt_ll_1 = rse1.log_likelihood
    opt_ll_2 = rse2.log_likelihood

    x = arange(0.05,0.95,0.005)
    f_1 = []
    f_2 = []
    err_1 = []
    err_2 = []
    
    for i in x:
        f1,err1 = rse1.predict(i)
        f2,err2 = rse2.predict(i)
        f_1.append(f1)
        f_2.append(f2)
        err_1.append(err1)
        err_2.append(err2)
    
    thetas = arange(0,5,0.005)
    ll_1 = []
    ll_2 = []
    for i in thetas:
        rse1.thetas = array([i])
        rse1._calculate_log_likelihood()
        ll_1.append(rse1.log_likelihood)
        rse2.thetas = array([i])
        rse2._calculate_log_likelihood()
        ll_2.append(rse2.log_likelihood)

    py.figure(1)
    py.plot(thetas,ll_1)
    py.plot(thetas,ll_2)
    py.scatter(theta1,opt_ll_1)
    py.scatter(theta2,opt_ll_2)
    py.ylabel('Likelihood')
    py.legend(('Response 1','Response 2'))

    py.figure(2)
    py.subplot(211)
    py.plot(x,f_1)
    py.plot(x,f_2)
    py.subplot(212)
    py.plot(x,err_1)
    py.plot(x,err_2)
    
    py.show()