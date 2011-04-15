from openmdao.lib.datatypes.api import ListStr, Dict, Float

from openmdao.main.api import Component
from openmdao.main.interfaces import IComponent

class FloatBroadcaster(Component): 
    """Takes inputs and passes them directly to outputs
    to be broadcast out to other components"""
    
    names = ListStr(iotype="in",desc="names of the variables you want to broadcast from this component")
    
    def __init__(self,names,types={'default':Float}): 
        super(FloatBroadcaster,self).__init__()
        self._vars = []
        self.types = types
        self.names = names
        
        
    def __types_changed(self,old,new): 
        if self.names: 
            self._names_changed(self.names,self.names)
        else: 
            pass
        
    #code to create inputs and outputs when names is changed
    def _names_changed(self,old,new):
        for in_var,out_var in self._vars: 
            if self.parent:
                self.parent.disconnect('.'.join([self.name,in_var]))
                self.parent.disconnect('.'.join([self.name,out_var]))
            self.remove_trait(in_var)
            self.remove_trait(out_var)
        self._vars = []
        
        for name in new:     
            in_var = "%s_in"%name
            out_var = name
            self.add_trait(in_var, Float(iotype="in",low=-9e99,high=9e99))            
            self.add_trait(out_var, Float(iotype="out"))
            
            self._vars.append((in_var,out_var))
            
    def execute(self,*args,**kwargs): 
        for in_var,out_var in self._vars:
            val = getattr(self,in_var)
            setattr(self,out_var,val)
            
            
            
            
            
            
                        
             
            
           
        
            