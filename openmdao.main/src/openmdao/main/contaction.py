

class ContainerAction(object):
    """A class for handling doing/undoing of actions on Containers."""
    def do(self, scope):
        pass
    
    def undo(self, scope):
        pass
    
    
class AddAction(ContainerAction):
    def __init__(self, name, obj):
        self.name = name
        self.obj = obj
        
    def do(self, scope):
        scope.add(name, obj)
        
    def undo(self, scope):
        scope.remove(name)
        
        
class AddVarAction(ContainerAction):
    def __init__(self, name, var):
        self.name = name
        self.obj = var
        
    def do(self, scope):
        scope.add_trait(name, var)
        
    def undo(self, scope):
        scope.remove_trait(name)
        

class RenameAction(ContainerAction):
    def __init__(self, oldname, newname):
        self.oldname = oldname
        self.newname = newname
        
    def do(self, scope):
        obj = getattr(scope, self.oldname)
        scope.remove(self.oldname)
        scope.add(self.newname, obj)
        
    def undo(self, scope):
        obj = getattr(scope, self.newname)
        scope.remove(self.newname)
        scope.add(self.oldname, obj)

class ConnectAction(ContainerAction):
    def __init__(self, src, dest):
        self.src = src
        self.dest = dest
        
    def do(self, scope):
        scope.connect(self.src, self.dest)
        
    def undo(self, scope):
        scope.disconnect(self.src, self.dest)
        
class DisconnectAction(ContainerAction):
    def __init__(self, src, dest):
        self.src = src
        self.dest = dest
        
    def do(self, scope):
        scope.disconnect(self.src, self.dest)
        
    def undo(self, scope):
        scope.connect(self.src, self.dest)
        

    