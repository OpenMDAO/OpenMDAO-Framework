import ordereddict

from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.expreval import ExprEvaluator

from openmdao.lib.components.api import Broadcaster

class GlobalDesVar(object): 
    """Global Design Variable object.
    
    name: Str
        the name of the global design variable
    targets: ListSrt
        The component variables that the global design variable should refer to 
    low: Float
        lower limit allowed for the variable value
    high: Float
        upper limit allowed for the variable value
    scalar: Float (optional)
        When a driver sets the value of the target variable, this value will be first multiplied by this scalar 
    adder: Float (optional)
        When a driver sets the value of the target variable, this value will be first added to set value
    """
    
    def __init__(self,name,targets,low,high,scalar=1.0,adder=0.0): 
        self.name = name
        
        self.low = low
        self.high = high
        self.scalar = scalar
        self.adder = adder
        
        self.targets = targets
        
        
        
    def __eq__(self,other): 
        if isinstance(other,self.__class__): 
            return all([self.targets==other.targets,
                        self.name==other.name,
                        self.low==other.low,
                        self.high==other.high,
                        self.scalar==other.scalar,
                        self.adder==other.adder])
        return False

class HasGlobalDesVars(object): 
    """This class provides an implementation of teh IHasGlobalDesVars interface
    
    parent: Assembly
        containing assembly where the HasGlobalDesVars lives. 
    """
    
    def __init__(self,parent): 
        self._des_vars = ordereddict.OrderedDict()
        self._parent = parent
        
    def add_global_des_var(self,name,targets,low,high,scalar=1.0,adder=0.0):
        """adds a global design variable to the assembly"""
        if name in self._des_vars: 
            self._parent.raise_exception("A global design variable named '%s' already exists in this assembly"%name,ValueError)
        
        exprs = []    
        for target in targets: 
            expr = ExprEvaluator(target,self._parent)
            if not expr.is_valid_assignee():
                self._parent.raise_exception("Cant add global design variable '%s' because the target '%s' refers to multiple assignees"%(name,target),
                                             ValueError)
            exprs.append(expr) 
            
        gdv =  GlobalDesVar(name,exprs,low,high,scalar,adder)
        self._des_vars[name] = gdv
        
    
    def remove_global_des_var(self,name): 
        """removed the global design variable from the assembly"""
        if name not in self._des_vars: 
            self._parent.raise_exception("No global design variable names '%s' exists in this assembly"%name,ValueError)
        gdv = self._des_vars[name]
        del self._des_vars[name]
        return gdv
    
    def clear_global_des_vars(self): 
        """removes all global design variables from the assembly"""
        self._des_vars = ordereddict.OrderedDict()
        
    def list_global_des_vars(self): 
        """returns a list of all the names of global design variable objects in the assembly"""
        return sorted(self._des_vars.keys())
    
    def get_global_des_vars(self,name=None): 
        """returns and ordered dict of global design variable objects in the assembly
        key: str (optional)
            if provided, function returns the local design variable matching the name
        """
        if name is not None and name in self._des_vars: 
            return self._des_vars[name]
        elif name is not None: 
            self._parent.raise_exception("No global design variable named '%s' "
                                         "has been added to the assembly"%name,ValueError)
        return self._des_vars
        
    #TODO: How do I handle calling of this method more than once? Need to clear out old broadcaster
    #    break any connections, and remove any possible parameters
    def setup_global_broadcaster(self,bcast_name,drivers=[]): 
        """creates a broadcaster with all the global design variables in the assembly.
        Calls add_parameter for all inputs in broadcaster on each driver specified
        
        name: string
            The name that should be used for the new broadcaster object
        drivers: List of Drivers (optional)
            The drivers which the global design variables will be added to as parameters
        """
        
        #make a broadcaster for the globals
        glb_names = self._des_vars.keys()
        self._parent.add(bcast_name,Broadcaster(glb_names))
        
        #connect the broadcast outputs to the disciplines
        # and add the broadcast parameters to the driver
        for gdv_name,gdv in self._des_vars.iteritems(): 
            for expr in gdv.targets:
                #This is just an initialization needed for the broadcaster, related to a HasParameters Bug 
                self._parent.set('%s.%s_in'%(bcast_name,gdv_name),self._parent.get(expr.text)) 
                
                self._parent.connect('%s.%s'%(bcast_name,gdv_name),expr.text) 
            for driver in drivers:    
                driver.add_parameter('%s.%s_in'%(bcast_name,gdv_name),low=gdv.low,high=gdv.high)    
        
class LocalDesVar(object): 
    """Local design variable object
    
    target: str (optional)
        name of the component variable that the local design variable references
    low: float (optional)
        minimum allowed value for the local design variable
    high: float (optional)
        maximum allowed value for the local design variable
    scalar: float (optional)
        When a driver sets the value of the target variable, this value will be first multiplied by this scalar
    addar: float (optional)
        When a driver sets the value of the target variable, this value will be first added to set value
    """
    
    def __init__(self,target="",low=None,high=None,scalar=None,addar=None): 
        self.target = target
        self.low = low
        self.high = high  
        self.scalar = scalar
        self.adder=adder
        
class HasLocalDesVars(object): 
    """This class provides an implementation of the IHasLocalDesVar interface
    
    parent: Assembly
        containing assembly where the HasGlobalDesVars lives. 
    """
    
    def __init__(self,parent): 
        self._parent = parent
        self._des_vars = ordereddict.OrderedDict()
        
    def add_local_des_var(self,target,low=None,high=None,scalar=1.0,adder=0):
        """adds a local design variable to the assembly"""
        
        if target in self._des_vars: 
            self._parent.raise_exception('A LocalDesVar with target "%s" has already been '
                                         'added to this assembly',ValueError)
         
        ldv = LocalDesVar(target,low,high,scalar,adder)
        self._des_vars[target] = ldv
        return ldv
        
    def remove_local_des_var(self,target):
        """removes the local design variable from the assembly"""
        
        if target not in self._des_vars: 
            self._parent.raise_exception('No local design variable named "%s"'
                                         ' has been added to the assembly'%target,ValueError)
        ldv = self._des_vars[target]
        del self._des_vars[target]
        return ldv 
    
    def list_local_des_vars(self): 
        """returns a list of all the names of the local design variables in the assembly"""
        return sorted(self._des_vars.keys())
    
    def get_local_des_vars(self,target=None): 
        """returns an ordered dictionary of all the local des vars in the assembly
        target: str (optional)
            if provided, returns the local design variable with a target matching the given key"""
        if target is not None and target in self._des_vars: 
            return self._des_vars[target]
        elif target is not None: 
            self._parent.raise_exception("No local design variable named '%s' "
                                         "has been added to the assembly"%target,ValueError)
        return self._des_vars    
    
    def clear_local_des_vars(self):
        """clears all local design variables from the assembly"""
        self._des_vars = ordereddict.OrderedDict()

class CouplingVar(object): 
    
    def __init__(self,indep,expr): 
        self.indep = indep
        self.expr = expr
        
    def __eq__(self,other): 
        return (self.indep==other.indep and self.expr==other.expr)
    
    def __str__(self): 
        return "(%s,%s)"%(self.indep,self.expr)
    
    def __repr__(self): 
        return "<CouplingVar(%s,%s)>)"%(self.indep,self.expr)

class HasCouplingVars(object):
    """This class provides an implementation of the IHasCouplingVar interface 
    
    parent: Assembly
        assembly object that this object belongs to
    """
    
    def __init__(self,parent):
        self._parent = parent
        self._indeps= ordereddict.OrderedDict()
        self._has_constraints = HasConstraints(self._parent)
        
    def add_coupling_var(self,indep,constraint,tollerance=.0001,scalar=1.0,adder=0.0):
        """adds a new coupling var to the assembly
        indep: str
            name of the independent variable, or the variable that should be varied, to meet the coupling 
            constraint
        constraint: str
            constraint equation, meeting the requirements of the IHasConstraints interface, which must be met 
            to enforce the coupling
        tolerance: float (optional)
            default value of .0001, specifies the tolerance to which the coupling constraint must be met to be 
            statisfied
        scalar: float (optional)
            default value of 1.0, specifies the scalar value that the constraint equation will be multiplied by 
            before being returned
        adder: float (optional)
            default value of 0.0, specifies the value which will be added to the constraint before being returned
        """
        cpl = CouplingVar(indep,constraint)
        #cant have any coupling variable with duplicate indep or constraint equations
        if indep not in self._indeps:
            self._indeps[indep] = constraint
            try: 
                #TODO, constraint tolerance???
                self._has_constraints.add_constraint(constraint,scalar,adder)
            except ValueError as err: 
                self._parent.raise_exception("A coupling variable with the "
                                             "constraint of the form '%s' already exists "
                                             "in the Aseembly"%constraint, ValueError)
            
        elif indep in self._indeps:
            self._parent.raise_exception("A coupling variable with indep of '%s' already "
                                         "exists in the assembly"%indep,ValueError) 
            
    def remove_coupling_var(self,indep):
        """removes the coupling var, idenfied by the indepent name, from the assembly. 
        
        indep: str 
            name of the independent variable from the CouplingVar   
        """
        if indep in self._indeps: 
            expr = self._indeps[indep]         
            self._constraints.remove(expr)
            
        else: 
            self._parent.raise_exception("No coupling variable with the indep '%s' has been "
                                         "added to the assembly"%indep,ValueError)
        
    def list_coupling_vars(self): 
        """returns a ordered list of names of the coupling vars in the assembly"""
        return sorted(self._indeps.keys())
    
    def get_coupling_vars(self,indep=None): 
        """returns an ordered dictionary of coupling vars, keyed to the name of 
        the independent associated with each one
        
        indep:str (optional)
            if provided, returns the coupling variable associated with the independent given
        """
            
        if indep is not None and indep in self._indeps: 
            return self._indeps[indep]
        
        elif indep is not None: 
            self._parent.raise_exception("No couling variable with indep '%s' "
                                         "exists in the assembly"%indep,ValueError)
        return self._indeps
    
    def clear_coupling_cars(self): 
        """removes all coupling variables from the assembly"""
        self._indeps = ordereddict.OrderedDict()
        self._has_constraints.clear_constraints()
        
     
        