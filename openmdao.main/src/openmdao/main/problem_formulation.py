import weakref

import ordereddict

from openmdao.main.api import Interface, ExprEvaluator, Assembly, Slot
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import IArchitecture, implements, IHasConstraints,IHasParameters,IHasCouplingVars,IHasObjectives

from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters, Parameter, ParameterGroup
from openmdao.main.hasobjective import HasObjectives


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
            name of the independent variable, or the variable 
            that should be varied, to meet the coupling constraint
        dep: str
            name of the dependent variable, or the variable that 
            needs to be forced to be consistent with the independent    
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

        
@add_delegate(HasConstraints,HasParameters,HasCouplingVars,HasObjectives)
class ArchitectureAssembly(Assembly): 
    implements(IHasConstraints,IHasParameters,IHasCouplingVars,IHasObjectives)
    
    architecture = Slot(IArchitecture,
                        desc="Slot for automatic architecture configurations")
    
    def get_expr_scope(self):
        """Return the scope to be used to evaluate ExprEvaluators."""
        return self

    def _architecture_changed(self, old, new): 
        if old is None or not old.configured:
            self.architecture.parent = self
        else:
            self._trait_change_notify(False)
            try:
                self.architecture = old  # put the old value back
            finally:
                self._trait_change_notify(True)
            self.raise_exception("This Assembly was already configured with another "
                                 "architecture.", RuntimeError)
    
    def configure(self): 
        self.architecture.check_config()
        self.architecture.configure()
        self.architecture.configured = True
    
    def check_config(self):
        super(ArchitectureAssembly, self).check_config()
        if self.architecture is not None:
            if self.architecture.configured:
                self.architecture.check_config()
            else:
                self.configure()
                
    def get_local_des_vars_by_comp(self): 
        """Return a dictionary of component names/list of parameters for 
        all single target parameters."""
        comps = {}
        for k,v in self.get_parameters().items():
            if isinstance(v, Parameter): 
                comp_names = v.get_referenced_compnames()
                if len(comp_names) > 1: 
                    continue
                
                comp = comp_names.pop()
                try: 
                    comps[comp].append(v)
                except KeyError: 
                    comps[comp] = [v]
        
        return comps
        
    def get_local_des_vars(self):
        """Return a list of single target Parameters."""
        return [(k,v) for k,v in self.get_parameters().items() 
                                        if isinstance(v, Parameter)]
    
    def get_global_des_vars(self): 
        """Return a list of multi target Parameters."""
        return [(k,v) for k,v in self.get_parameters().items() 
                                        if isinstance(v, ParameterGroup)]
        
