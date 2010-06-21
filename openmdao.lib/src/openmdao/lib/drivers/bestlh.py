from __future__ import division

import random

from numpy import array,zeros,size,argsort,sum,unique,floor,equal,bincount
from numpy.linalg import norm

from enthought.traits.api import Event

from openmdao.main.api import Driver, ExprEvaluator
from openmdao.lib.api import Float,Int, Enum
from openmdao.util.mdo import rand_latin_hypercube

class DesVar(object): 
    
    def __init__(self): 
        self.low = None
        self.high = None
        self.expr = None

class LatinHypercube(object):
    def __init__(self,doe,q=2,p=1):
        self.q = q
        self.p = p
        self.doe = doe
        
        self.phi = 0
    
    def get_shape(self):
        return self.doe.shape
    
    shape = property(get_shape,None,None,"(rowsxcolumns) size of the LatinHypercube doe")
        
    def mmphi(self):
        """Calculates the Morris-Mitchell sampling criterion for input DOE"""
        n = size(self.doe,0)
        d = []
        
        #calculate the norm between each pair of points in the DOE
        for i,row_a in enumerate(self.doe):
            for j,row_b in enumerate(self.doe):
                #check for distance between same point, always = 0. Also avoid duplicate calcs
                if i==j or j>i: continue
                
                else: d.append(norm(row_a-row_b,ord = self.p))
        
        #toss out any entries with the same distance
        distinct_d = unique(d)
        
        #mutltiplicity array with a count of how many pairs of points have a given distance
        J = [d.count(x) for x in distinct_d]
        
        phiQ = sum(J*(distinct_d**(-self.q)))**(1.0/self.q)
        
        return phiQ  
        
    def perturb(self,mutation_count):
        """ Interchanges pairs of randomly chosen elements within randomly chosen
        columns of a doe a number of times. The result of this operation will also 
        be a Latin hypercube.
        """
        new_doe = self.doe.copy()
        n,k = self.doe.shape
        for count in range(mutation_count): 
            col = random.randint(0,k-1)
            
            #choosing two distinct random points
            el1 = 1; el2 = 1;
            while el1==el2: 
                el1 = random.randint(0,n-1)
                el2 = random.randint(0,n-1)
           
            new_doe[el1,col] = self.doe[el2,col]
            new_doe[el2,col] = self.doe[el1,col] 
               
        return LatinHypercube(new_doe,self.q,self.p)
            
    def __repr__(self): 
        return repr(self.doe)
    
    def __str__(self): 
        return str(self.doe)
    
    def __getitem__(self,*args): 
        return self.doe.__getitem__(*args)


class BestLatinHypercube(Driver): 
    
    n = Int(20,iotype="in",desc="number of sample points in the DOE")
    
    population = Int(20,iotype="in",
                     desc="Size of the population for the genetic algorithm optimization of the DOE sample points")
    generations = Int(2,iotype="in",
                      desc="Number of generations the genetic algorithm will evolve for before terminating")
    norm_method = Enum(["1-norm","2-norm"],iotype='in',
                       desc="vector norm calculation method. '1-norm' is faster, but less accurate")
    norm_map = {"1-norm":1,"2-norm":2}
    
    def __init__(self, doc=None):
        super(BestLatinHypercube,self).__init__(doc)
        self._des_vars = {}
        self._event_vars = {}
        self.q = [1,2,5,10,20,50,100] #list of qs to try for Phi_q optimization
      
    def add_event_var(self, ref):
        """Adds an event variable to the driver, which the driver will the set
        before each iteration. 'ref' is string refering to the public event
        variable on some component. 
        """
        
        if ref in self.event_vars: 
            self.raise_exception("Trying to add event_var '%s' to driver, " % ref,
                                 "but it is already in there",RuntimeError)
        
        expreval = ExprEvaluator(ref,self.parent, single_name=True)
        path = ".".join(ref.split(".")[0:-1]) #get the path to the object
        target = ref.split(".")[-1] #get the last part of the string after the last "."

        obj = getattr(self.parent,path)
        t = obj.trait(target)
        if (not t) or (not t.is_trait_type(Event)):
            self.raise_exception("refence provided, '%s', does not point to an Event variable. Only Event variables are allowed"%ref,RuntimeError)
        
        self._event_vars[ref] = expreval
        

    def add_des_var(self, ref, low=None, high=None):
        """Adds a design variable to the driver. 'ref' is a string refering to the public variable the 
        driver should vary during execution. 'low' and 'high' refer to the minimum and maximum allowed 
        values for the optimizer to use. If neither are specified, the min and max will default to the 
        values in the metadata of the public variable being referenced. If they are not specified in 
        the metadata and not provided as arguments a ValueError is raised.
        """
        if ref in self.design_vars: 
            self.raise_exception("Trying to add '%s' to driver, but it is already in there" % ref,
                                 RuntimeError)
        
        expreval = ExprEvaluator(ref, self.parent, single_name=True)
        
        des_var = DesVar()
        des_var.expr = expreval
        
        path = ".".join(ref.split(".")[0:-1]) #get the path to the object
        target = ref.split(".")[-1] #get the last part of the string after the last "."

        obj = getattr(self.parent,path)
        t = obj.trait(target)
        trait_low_high = False
        if t and t.is_trait_type(Float):
            if hasattr(t,'low') and hasattr(t,'high'):
                trait_low_high = True
            if low!=None and high!=None:
                if trait_low_high:
                    if high > thigh or low < tlow:
                        self.raise_exception("Trying to add design variable '%s', " % ref,
                                             "but the low/high metadata supplied (%s, %s) exceeds the" % (low,high),
                                             "built-in low/high limits (%s, %s)." % (t.low,t.high),RuntimeError)
                _des_var.low = low
                _des_var.high = high
            elif trait_low_high:
                _des_var.low = t.low
                _des_var.high = t.high
            else: 
                self.raise_exception("Trying to add design variable '%s', but no low/high metadata was found and no" % ref,
                                     "'low','high' arguments were given. One or the other must be specified.",RuntimeError)
            # the des_var has been created, with expr and low/high. 
            # Just add it to the storage dictionary
            self.des_vars[ref] = _des_var
        else: 
            self.raise_exception("Trying to add design variable '%s' to driver, but only Float types are allowed."%ref,RuntimeError)
            
    def remove_event_var(self,ref): 
        if ref not in self._event_vars:
            self.raise_exception("Trying to remove event variable '%s' that is not in the driver."%ref,RuntimeError)
        del self._event_vars[ref]
        return True
        
    def remove_des_var(self,ref): 
        if ref not in self._des_vars:
            self.raise_exception("Trying to remove design variable '%s' that is not in the driver."%ref,RuntimeError)
        del self._des_vars[ref]
        return True
    
    def list_event_vars(self): 
        return sorted(self._event_vars.keys())
            
    def list_des_vars(self):
        return sorted(self._des_vars.keys())
    
    def clear_event_vars(self): 
        self._event_vars = dict()
        return True 
        
    def clear_des_vars(self): 
        self._des_vars = dict()
        return True
    
    def execute():
        k = len(self._des_vars)
        lhcs = []
        rand_doe = rand_latin_hypercube(n,k)
        
        for i in self.q:
            lh = LatinHypercube(rand_doe,q=i,p=self.norm_map[self.norm_method])
            lh_opt = _mmlhs(lh,self.population,self.generations)
            lhcs.append(lh_opt)
            
        best_lhc = _mmsort(lhcs,norm_map[norm_method])
        
        #run the cases
        for row in X:
            for val,ref,des_var in zip(row,self._des_vars.iteritems()):
                #convert DOE values to variable values and set values
                des_var.expr.set(des_var.low+(des_var.high-des_var.low)*val)
            
            for ref,event_var in self._event_vars.iteritems():
                event_var.set(1)  
            #run model
            self.run_iteration()

def _mmlhs(x_start,population,generations):
    """Evolutionary search for most space filling Latin-Hypercube. 
    Returns a new LatinHypercube instance with an optimzied set of points"""
    x_best = x_start
    phi_best = x_start.mmphi()
    n = x_start.shape[1]
    
    level_off = floor(0.85*generations)
    for it in range(generations): 
        
        if it < level_off:
            mutations = int(round(1+(0.5*n-1)*(level_off-it)/(level_off-1)))
        else: 
            mutations = 1
        x_improved = x_best
        phi_improved = phi_best
        
        for offspring in range(population):
            x_try = x_best.perturb(mutations)
            phi_try = x_try.mmphi()
            
            if phi_try < phi_improved: 
                x_improved = x_try
                phi_improved = phi_try
                
        if phi_improved < phi_best: 
            phi_best = phi_improved
            x_best = x_improved
    return x_best

def _mmsort(lhcs):
    #"""Ranks DOEs according to Morris-Mitchell criterion"""
    scores = []
    for lh in lhcs:
        scores.append(lh.mmphi())
    Index = argsort(scores)
    return lhcs(Index[0])



if __name__== "__main__": 
    import sys
    
    try:
        from matplotlib import pyplot
    except ImportError:
        print "Couldn't find matplotlib"
    
    test = """
x = rand_latin_hypercube(100,2)
lh = LatinHypercube(x,2,1) 
print lh.mmphi()
lh_opt = _mmlhs(lh,20,20)
print lh_opt.mmphi()
"""
    if '--profile' in sys.argv:
        import cProfile
        import pstats
        cProfile.run(test,"test.prof")
        p = pstats.Stats("test.prof")
        p.sort_stats('cumulative').print_stats(10)
    else:
        exec(test)
    if 'pyplot' in globals():
        pyplot.figure(1)
        pyplot.scatter(lh[:,0],lh[:,1])
        pyplot.scatter(lh_opt[:,0],lh_opt[:,1],c='r')
        pyplot.show()
    
    
