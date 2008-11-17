
#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.variable import Variable


class ContainerVariable(Variable):
    """A Variable class that refers to an IContainer object."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, default=None, desc=None):
        Variable.__init__(self, name, parent, iostatus, ref_name=ref_name, 
                          default=default, desc=desc)


    def _pre_assign(self, var, attribname=None):
        pass
    
        
    def _pre_connect(self, variable, attribname=None):
        pass

    
    def get(self, path):
        """Return the framework-accessible object specified by the given 
        path, which may contain '.' characters.
        
        """
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None
        try:
            scope = self._framework_objs[base]
        except KeyError:
            raise NameError(self.get_pathname()+": '"+path+
                                            "' not a framework-accessible object")
        if name is None:
            return scope
        else:
            return scope.get(name)

    def set(self, path, value):
        """Set the value of the framework-accessible object specified by the 
        given path, which may contain '.' characters.
        
        """ 
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None
        try:
            scope = self._framework_objs[base]
        except:
            raise NameError(self.get_pathname()+": '"+path+
                           "' not a framework-accessible object")
        
        scope.set(name, value)

