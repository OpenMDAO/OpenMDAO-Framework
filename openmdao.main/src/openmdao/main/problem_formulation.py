import ordereddict

from openmdao.main.api import Interface, ExprEvaluator, Assembly, Slot
from openmdao.main.hasconstraints import HasConstraints
from openmdao.util.decorators import add_delegate

from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective, HasObjectives

class IArchitecture(Interface):
    
    def __init__(self): 
        self.parent = None
        
    def configure(self): 
        """sets up drivers,workflows, and data connections in 
        the assembly to configure the architecture
        """        
        pass
    
    def clear(self): 
        """removes all the drivers, workflows, and data connections in the assembly, 
        leaving the assembly cleaned up. 
        """
        
        pass

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
            name of the dependent variable, or the variable that needs to be forced to be consistent with the 
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
    
@add_delegate(HasConstraints,HasParameters,HasObjective,HasCouplingVars,HasObjectives)
class ArchitectureAssembly(Assembly): 
    
    architecture = Slot(IArchitecture, iotype="in",
                        desc="Slot for the use of automatic architecture configurations")
    
    def _architecture_changed(self): 
        #TODO: When architecture is added, need to check to make sure it can
        #support all the types of stuff in the assembly. (single vs. multiple
        #objectives, constraints, all the variable types, etc.)
        self.architecture.parent = self
    
    def configure(self): 
        self.architecture.configure()
    
    
    def get_local_des_vars(self): 
        return [(k,v) for k,v in self.get_parameters().iteritems() 
                                        if not isinstance(k,tuple)]
    
    def get_global_des_vars(self): 
        return [(k,v) for k,v in self.get_parameters().iteritems() 
                                        if isinstance(k,tuple)]
        
     
        