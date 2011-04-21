import ordereddict

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
        scalar to be applied to the variable 
    adder: Float (optional)
        adder to be applie to the variable
    """
    
    def __init__(self,name,targets,low,high,scalar=None,adder=None): 
        self.name = name
        self.targets = targets
        self.low = low
        self.high = high
        self.scalar = scalar
        self.adder = adder

class HasGlobalDesVars(object): 
    """Delegate object which handles configuration of broadcasters and drivers for
    an Assembly, based on a set of GlobalDesVar objects
    
    parent: Assembly
        containing assembly where the HasGlobalDesVars lives. 
    """
    
    def __init__(self,parent): 
        self._des_vars = ordereddict.OrderedDict()
        self._parent = parent
        
    def add_global_des_var(self,name,targets,low,high,scalar=None,adder=None):
        """adds a global design variable to the assembly"""
        if name in self._des_vars: 
            self._parent.raise_exception("A global design variable named '%s' already exists",%name,ValueError)
        gdv =  GlobalDesVar(name,targets,low,high,scalar,adder)
        self._des_vars[name] = gdv
        return gdv
    
    def remove_global_des_var(self,name): 
        """removed the global design variable from the assembly"""
        if name not in self._des_vars: 
            self._parent.raise_exception("No global design variable names '%s' exists"%name,ValueError)
        gdv = self._des_vars[name]
        del self._des_vars[name]
        return gdv
    
    def clear_global_des_vars(self): 
        """removes all global design variables from the assembly"""
        self._des_vars = []
        
    def list_global_des_vars(self): 
        """returns a list of all the names of global design variable objects in the assembly"""
        return sorted(self._des_vars.keys())
    
    def get_global_des_vars(self): 
        """returns and ordered dict of global design variable objects in the assembly"""
        return self._des_vars
        
    def setup_global_broadcaster(self,bcast_name,drivers=None): 
        """creates a broadcaster with all the global design variables in the assembly.
        Calls add_parameter for all inputs in broadcaster on each driver specified
        
        name: string
            The name that should be used for the new broadcaster object
        drivers: List of Drivers (optional)
            The drivers which the global design variables will be added to as parameters
        """
        
        #make a broadcaster for the globals
        glb_names = self._des_vars.keys()
        self.add(bcast_name,Broadcaster(glb_names))
        
        #connect the broadcast outputs to the disciplines
        # and add the broadcast parameters to the driver
        for gdv_name,gdv in self._des_vars.iteritems: 
            for var in glb_var.vars:
                #This is just an initialization needed for the broadcaster
                self._parent.set('%s.%s_in'%(bcast_name,gdv_name),self._parent.get(var)) 
                self._parent.connect('%s.%s'%(bcast_name,gdv_name),var) 
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
        scalar to be applied to the variable 
    addar: float (optional)
        adder to be applied to the variable
    """
    
    def __init__(self,target="",low=None,high=None,scalar=None,addar=None): 
        self.target = target
        self.low = low
        self.high = high  
        self.scalar = scalar
        self.adder=adder
        
class HasLocalDesVar(object): 
    """Delegate object which handles configuration of drivers for
    an Assembly, based on a set of LocalDesVar objects
    
    parent: Assembly
        containing assembly where the HasGlobalDesVars lives. 
    """
    
    def __init__(self,parent): 
        self._parent = parent
        self._des_vars = ordereddict.OrderedDict()
        
    def add_local_des_var(self,target,low=None,high=None,scalar=None,adder=None):
        if target in self._des_vars: 
            self._parent.raise_exception('A LocalDesVar with target "%s" has already been '
                                         'added to this assembly',ValueError)
         
        ldv = LocalDesVar(target,low,high,scalar,adder)
        self._des_vars[target] = ldv
        return ldv
        
    def remove_local_des_var(self,target):
        if target not in self._des_vars: 
            self._parent.raise_exception('No local design variable named "%s"'
                                         ' has been added to the assembly'%target,ValueError)
        ldv = self._des_vars[target]
        del self._des_vars[target]
        return ldv 
    
    def list_local_des_vars(self): 
        return sorted(self._des_vars.keys())
    
    def clear_local_des_vars(self):
        self._des_vars = []
        
class CouplingVar(object): 
    def __init__(self): 
        self.vary = None
        self.constraint= None
            
            
        