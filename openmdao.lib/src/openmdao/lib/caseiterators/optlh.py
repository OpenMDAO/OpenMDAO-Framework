# This implementation is based on a matlab implementation that has the
# following license:
#
# Copyright 2007 A Sobester
#
# This program is free software: you can redistribute it and/or modify  it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or any
# later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License and GNU
# Lesser General Public License along with this program. If not, see
# <http://www.gnu.org/licenses/>.

from __future__ import division

import random

from numpy import array,zeros,size,argsort,unique,sum,floor,equal,bincount,sqrt,diff
from numpy.linalg import norm

from enthought.traits.api import HasTraits, Event, implements

from openmdao.main.api import ExprEvaluator
from openmdao.main.interfaces import ICaseIterator
from openmdao.lib.api import Float,Int, Enum
from openmdao.util.mdo import rand_latin_hypercube

import time

def print_timing(func):
    def wrapper(*arg):
        t1 = time.time()
        res = func(*arg)
        t2 = time.time()
        print '%s took %0.3f ms' % (func.func_name, (t2-t1)*1000.0)
        return res
    return wrapper


class _DesVar(object): 
    
    def __init__(self): 
        self.low = None
        self.high = None
        self.expr = None

class LatinHypercube(object):
    def __init__(self, doe, q=2, p=1):
        self.q = q
        self.p = p
        self.doe = doe
        
        self.phi = 0
    
    def get_shape(self):
        return self.doe.shape
    
    shape = property(get_shape,None,None,"(rowsxcolumns) size of the LatinHypercube DOE.")
        
    def mmphi(self):
        """Calculates the Morris-Mitchell sampling criterion for input DOE."""
        n,m = self.doe.shape
        distdict = {}
        
        #calculate the norm between each pair of points in the DOE
        arr = self.doe
        for i in range(n):
            for j in range(i+1, n):
                nrm = norm(arr[i]-arr[j], ord=self.p)
                distdict[nrm] = distdict.get(nrm, 0) + 1

        distinct_d = array(distdict.keys())
        
        #mutltiplicity array with a count of how many pairs of points have a given distance
        J = array(distdict.values())
        
        phiQ = sum(J*(distinct_d**(-self.q)))**(1.0/self.q)
        
        return phiQ  
    
    #def mmphi(self):
        #"""Calculates the Morris-Mitchell sampling criterion for input DOE"""
        #n,m = self.doe.shape
        #d = []
        
        ##calculate the norm between each pair of points in the DOE
        #for i,row_a in enumerate(self.doe):
            #for j,row_b in enumerate(self.doe):
                ##check for distance between same point, always = 0. Also avoid duplicate calcs
                #if i==j or j>i: continue

                #else: d.append(norm(row_a-row_b,ord = self.p))
                    
        ##toss out any entries with the same distance
        #distinct_d = unique(d)
        
        ##mutltiplicity array with a count of how many pairs of points have a given distance
        #J = array([d.count(x) for x in distinct_d])
        
        #phiQ = sum(J*(distinct_d**(-self.q)))**(1.0/self.q)
        
        #return phiQ
        
    def perturb(self,mutation_count):
        """Interchanges pairs of randomly chosen elements within randomly chosen
        columns of a DOE a number of times. The result of this operation will also 
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

_norm_map = {"1-norm":1,"2-norm":2}

class OptLatinHypercube(HasTraits): 
    
    implements(ICaseIterator)
    
    n = Int(20, desc="number of sample points in the DOE")
    
    population = Int(20,
                     desc="Size of the population for the genetic algorithm optimization of the DOE sample points")
    generations = Int(2,
                      desc="Number of generations the genetic algorithm will evolve for before terminating")
    norm_method = Enum(["1-norm","2-norm"],
                       desc="vector norm calculation method. '1-norm' is faster, but less accurate")
    
    def __init__(self):
        super(OptLatinHypercube,self).__init__()
        self._des_vars = {}
        self._event_vars = {}
        self.q = [1,2,5,10,20,50,100] #list of qs to try for Phi_q optimization
      
    def add_event_var(self, varname):
        """Adds an event variable to the driver, which the driver will the set
        before each iteration. 
                
        varname : string
            Name of the public event variable that should be set before execution.
        """
        
        if varname in self.event_vars: 
            self.raise_exception("Trying to add event_var '%s' to driver, " % varname,
                                 "but it is already in there",RuntimeError)
        
        expreval = ExprEvaluator(varname,self.parent, single_name=True)
        path = ".".join(varname.split(".")[0:-1]) #get the path to the object
        target = varname.split(".")[-1] #get the last part of the string after the last "."

        obj = getattr(self.parent,path)
        t = obj.trait(target)
        if (not t) or (not t.is_trait_type(Event)):
            self.raise_exception("refence provided, '%s', does not point to an Event variable. Only Event variables are allowed"%varname,RuntimeError)
        
        self._event_vars[varname] = expreval
        

    def add_des_var(self, varname, low=None, high=None):
        """Adds a design variable to the driver. 
        
        varname : string
            Name of the public variable the driver should vary during execution.
            
        low : float, optional
            Minimum allowed value of the design variable.
            
        high : float, optional
            Maximum allowed value of the design variable.
        
        If neither 'low' nor 'high' is specified, the min and max will
        default to the values in the metadata of the public variable being
        referenced. If they are not specified in the metadata and not provided
        as arguments a ValueError is raised.
        """
        if varname in self.design_vars: 
            self.raise_exception("Trying to add '%s' to driver, but it is already in there" % varname,
                                 RuntimeError)
        
        expreval = ExprEvaluator(varname, self.parent, single_name=True)
        
        des_var = _DesVar()
        des_var.expr = expreval
        
        path = ".".join(varname.split(".")[0:-1]) #get the path to the object
        target = varname.split(".")[-1] #get the last part of the string after the last "."

        obj = getattr(self.parent,path)
        t = obj.trait(target)
        trait_low_high = False
        if t and t.is_trait_type(Float):
            if hasattr(t,'low') and hasattr(t,'high'):
                trait_low_high = True
            if low!=None and high!=None:
                if trait_low_high:
                    if high > thigh or low < tlow:
                        self.raise_exception("Trying to add design variable '%s', " % varname,
                                             "but the low/high metadata supplied (%s, %s) exceeds the" % (low,high),
                                             "built-in low/high limits (%s, %s)." % (t.low,t.high),RuntimeError)
                _des_var.low = low
                _des_var.high = high
            elif trait_low_high:
                _des_var.low = t.low
                _des_var.high = t.high
            else: 
                self.raise_exception("Trying to add design variable '%s', but no low/high metadata was found and no" % varname,
                                     "'low','high' arguments were given. One or the other must be specified.",ValueError)
            # the des_var has been created, with expr and low/high. 
            # Just add it to the storage dictionary
            self.des_vars[varname] = _des_var
        else: 
            self.raise_exception("Trying to add design variable '%s' to driver, but only Float types are allowed."%varname,TypeError)
            
    def remove_event_var(self,varname): 
        if varname not in self._event_vars:
            self.raise_exception("Trying to remove event variable '%s' that is not in the driver."%varname,RuntimeError)
        del self._event_vars[varname]
        return True
        
    def remove_des_var(self,varname): 
        if varname not in self._des_vars:
            self.raise_exception("Trying to remove design variable '%s' that is not in the driver."%varname,RuntimeError)
        del self._des_vars[varname]
        return True
    
    def list_event_vars(self): 
        return sorted(self._event_vars.keys())
            
    def list_des_vars(self):
        return sorted(self._des_vars.keys())
    
    def clear_event_vars(self): 
        self._event_vars = {}
        return True 
        
    def clear_des_vars(self): 
        self._des_vars ={}
        return True
    
    def __iter__(self):
        """Return an iterator over our set of Cases"""
        k = len(self._des_vars)
        lhcs = []
        rand_doe = rand_latin_hypercube(n,k)
        
        for i in self.q:
            lh = LatinHypercube(rand_doe,q=i,p=_norm_map[self.norm_method])
            lh_opt = _mmlhs(lh,self.population,self.generations)
            lhcs.append(lh_opt)
            
        best_lhc = _mmsort(lhcs,_norm_map[norm_method])
        
        #run the cases
        for row in X:
            for val,varname,des_var in zip(row,self._des_vars.iteritems()):
                #convert DOE values to variable values and set values
                des_var.expr.set(des_var.low+(des_var.high-des_var.low)*val)
            
            for varname,event_var in self._event_vars.iteritems():
                event_var.set(1)

            #run model
            self.run_iteration()

@print_timing
def _mmlhs(x_start, population, generations):
    """Evolutionary search for most space filling Latin-Hypercube. 
    Returns a new LatinHypercube instance with an optimized set of points."""
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
            #print 'gen,pop,mut = ',it,offspring,mutations
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
x = rand_latin_hypercube(80,2)
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
