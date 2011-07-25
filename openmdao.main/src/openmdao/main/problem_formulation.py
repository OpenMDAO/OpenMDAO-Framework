import weakref

import ordereddict

from openmdao.main.api import Interface, ExprEvaluator, Assembly, Slot
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import IArchitecture

from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters, Parameter, ParameterGroup
from openmdao.main.hasobjective import HasObjectives




class CouplingVar(ExprEvaluator): 
    
    @property
    def target(self): 
        return self.text
    
    @property
    def targets(self): 
        return [self.text]
    
    @property
    def low(self): 
        return self.get_metadata('low')[0][1]
    
    @property
    def high(self):
        return self.get_metadata('high')[0][1]

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
        expr_indep = CouplingVar(indep,self._parent)
        if not expr_indep.check_resolve() or not expr_indep.is_valid_assignee():
                self._parent.raise_exception("Cant add coupling variable with indep '%s' "
                                             "because is not a valid variable"%indep,
                                             ValueError)
                
        expr_dep = CouplingVar(dep,self._parent)
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
                
        self._couples.append((expr_indep,expr_dep))    
    
            
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
        
    def get_coupling_vars(self): 
        """returns a ordered list of (indep,dep) pairs of CouplingVar instances in the assembly"""
        return self._couples
    
    def clear_coupling_vars(self): 
        """removes all coupling variables from the assembly"""
        self._couples = []    

        
@add_delegate(HasConstraints,HasParameters,HasCouplingVars,HasObjectives)
class ArchitectureAssembly(Assembly): 
    
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
    
    def get_global_des_vars_by_comp(self): 
        """Return a dictionary of component names/list of parameters for 
        all multi target parameters."""
        result = {}
        for k,v in self.get_parameters().items():
            if isinstance(v, ParameterGroup): 
                data = v.get_referenced_vars_by_compname()
                for name,vars in data.iteritems(): 
                    try: 
                        result[name].extend(vars)
                    except KeyError: 
                        result[name] = list(vars)
        
        return result    
    
    def get_global_des_vars(self): 
        """Return a list of multi target Parameters."""
        return [(k,v) for k,v in self.get_parameters().items() 
                                        if isinstance(v, ParameterGroup)]
    
    
    def get_des_vars_by_comp(self): 
        """Return a dictionary of component names/ list of parameters fo all 
        parameters (global and local)""" 
        
        result = self.get_local_des_vars_by_comp()
        for k,v in self.get_global_des_vars_by_comp().iteritems(): 
            try: 
                result[k].extend(v)
            except KeyError: 
                result[k] = v
                
        return result
    
    def get_coupling_indeps_by_comp(self): 
        """Returns a dictionary of coupling var independents 
        keyed to the component they are part of""" 
        
        result = {}
        for indep,dep in self.get_coupling_vars(): 
            comp = indep.get_referenced_compnames().pop()
            try: 
                result[comp].append(indep)
            except KeyError: 
                result[comp] = [indep]
                
        return result        
                
    def get_coupling_deps_by_comp(self): 
        """Returns a dictionary of coupling var independents 
        keyed to the component they are part of""" 
        
        result = {}
        for indep,dep in self.get_coupling_vars(): 
            comp = dep.get_referenced_compnames().pop()
            try: 
                result[comp].append(dep)
            except KeyError: 
                result[comp] = [dep]            
        
        return result
    
    def get_constraints_by_comp(self):
        result = {}
        for text,const in self.get_constraints().iteritems(): 
            comps = const.get_referenced_compnames()
            for comp in comps: 
                try: 
                    result[comp].append(const)
                except: 
                    result[comp] = [const,]
        return result            