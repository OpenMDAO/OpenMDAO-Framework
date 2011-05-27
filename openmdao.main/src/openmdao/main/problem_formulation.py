import ordereddict

from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.expreval import ExprEvaluator

from openmdao.lib.components.api import Broadcaster


class CouplingVar(object): 
    
    def __init__(self,indep, dep): 
        self.indep = indep
        self.dep = dep

class HasCouplingVars(object):
    """This class provides an implementation of the IHasCouplingVar interface 
    
    parent: Assembly
        assembly object that this object belongs to
    """
    
    def __init__(self,parent):
        self._parent = parent
        self._couples = []
        
    def add_coupling_var(self,indep,dep):
        """adds a new coupling var to the assembly
        
        indep: str
            name of the independent variable, or the variable that should be varied, to meet the coupling 
            constraint
        dep: str
            name of the dependent variable, or the variable that needs to be forced to be consitent with the 
            independent    
        """
        expr_indep = ExprEvaluator(indep,self._parent)
        if not expr_indep.check_resolve() or not expr_indep.is_valid_assignee():
                self._parent.raise_exception("Cant add coupling variable with indep '%s' "
                                             "because is not a valid variable"%indep,
                                             ValueError)
                
        expr_dep = ExprEvaluator(dep,self._parent)
        if not expr_indep.check_resolve() or not expr_indep.is_valid_assignee():
                self._parent.raise_exception("Cant add coupling variable with dep '%s' "
                                             "because is not a valid variable"%dep,
                                             ValueError)        
        if self._couples: 
            indeps,deps = zip(*self._couples)
            if indep in indeps:
                self._parent.raise_exception("Coupling variable with indep '%s' already "
                                             "exists in assembly"%indep,ValueError)    
            if dep in deps:
                self._parent.raise_exception("Coupling variable with dep '%s' already "
                                             "exists in assembly"%dep,ValueError)
                
        self._couples.append((indep,dep))    
    
            
    def remove_coupling_var(self,couple):
        """removes the coupling var, indep/dep pair from the assembly. 
        
        couple: tuple of str 
            two tuple of (<indep>,<dep>) to be removed
        """
        try: 
            self._couples.remove(couple)
        except: 
            self._parent.raise_exception("No coupling variable of ('%s','%s') exists "
                                         "in assembly"%couple,ValueError)
        
    def list_coupling_vars(self): 
        """returns a ordered list of names of the coupling vars in the assembly"""
        return self._couples
    
    
    def clear_coupling_vars(self): 
        """removes all coupling variables from the assembly"""
        self._couples = []
        
     
        