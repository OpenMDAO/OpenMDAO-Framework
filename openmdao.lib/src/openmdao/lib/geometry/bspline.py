import cPickle
import os.path

from numpy import linspace, hstack, dstack, less ,less_equal, logical_and, \
    array, empty, matrix, dot
    
from scipy.optimize import fsolve, newton
from scipy.sparse import csr_matrix

class Bspline(object): 
    def __init__(self,controls,points,order=3): #controls and points are 2-d arrays of points 

        self.controls = controls
        self.order = order
        self.degree = order-1
        self.n = len(controls)
        self.knots =  hstack(([0,]*(self.degree),
                              hstack((linspace(0,1,self.n-self.order+2),[1,]*(self.degree)))
                             ))
        self.__b_cache = {} #uses for memoizing the b_jn function
        self.max_x = max(points[:,0]) 

        #see if we can 
        h1 = str(hash(tuple(points.flatten()))).replace("-","n")
        h2 = str(hash(tuple(controls.flatten()))).replace("-","n")
        pkl_file_name = "%s__%s.bspline_pkl"%(h1,h2)
        pkl_folder = "pyBspline_pkl"
        pkl_file_name = os.path.join(pkl_folder,pkl_file_name)
        if not os.path.exists(pkl_folder): 
            os.mkdir(pkl_folder)
        if os.path.exists(pkl_file_name): 

            self.B = cPickle.load(open(pkl_file_name))
        else: 
            self.B = self._calc_jacobian(points)
            cPickle.dump(self.B,open(pkl_file_name,'w'))

   
    def _calc_jacobian(self,points):                       
        #pre-calculate the B matrix
        n_p = points.shape[0]
        B = matrix(empty((n_p,self.n))) #1 row per point, one column per control_point
        
        r = range(0,self.n)
        for i,p in enumerate(points): 
            t = self.find(p[0])
            for j in r: 
                B[i,j] = self.b_jn_wrapper(j,self.degree,t)    
                
        #self.B = csr_matrix(B)
        self.B = B
        return self.B
                    
    def calc(self,C,points=None):
        self.controls = C
        if points: 
            self.B = self._calc_jacobian(points)
            
        return array(self.B.dot(C))     
                    
     
    def find(self,X):
        """returns the parametric coordinate that matches the given x location""" 
        
        try:     
            return array([fsolve(lambda f: self(f)[:,0][0] - x,[x/self.max_x,],xtol=1e-5) for x in X])[:,0]
        except TypeError:
            return fsolve(lambda f: self(f)[:,0] - X,[X/self.max_x,],xtol=1e-5)
                 
        
    def b_jn(self,j,n,t):         
        t_j   = self.knots[j]
        t_j1  = self.knots[j+1]
        t_jn  = self.knots[j+n]
        t_jn1 = self.knots[j+n+1]
        
        if n==0:       
            return logical_and(less_equal(t_j,t),less(t,t_j1))                
        
        if t_jn-t_j:     
            q1 = (t-t_j)/(t_jn-t_j)
        else: 
            q1 = 0    
            
        if t_jn1-t_j1: 
            q2 = (t_jn1-t)/(t_jn1-t_j1)
        else: 
            q2 = 0
                
        B = q1*self.b_jn(j,n-1,t) + q2*self.b_jn(j+1,n-1,t)   
        
        return B
        
    #dirty hack to fix some weird corner case that shows up    
    def b_jn_wrapper(self,j,n,t): 
        tuple_t = tuple(t)
        try: 
            return self.__b_cache[(j,n,tuple_t)]
        except KeyError:     
            B = self.b_jn(j,n,t)
            if j == self.n-1: 
                try: 
                    B[t==1]=1 #anywhere t=1, set B = 1
                except TypeError: 
                    B = 1   
            self.__b_cache[(j,n,tuple_t)] = B         
            return B 
        
    def __call__(self,t): 
        rng = range(0,self.n)
        b = [self.b_jn_wrapper(i,self.degree,t) for i in rng]
        X = dot(self.controls[:,0],b)
        Y = dot(self.controls[:,1],b)
        return dstack((X,Y))[0]     
        