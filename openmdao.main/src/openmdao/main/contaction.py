
import copy

class ContainerAction(object):
    """A class for handling doing/undoing of actions on Containers."""
    def do(self, scope):
        raise NotImplementedError("do function not implemented")
    
    def undo(self, scope):
        raise NotImplementedError("undo function not implemented")
    
class AddAction(ContainerAction):
    def __init__(self, name, obj):
        self.name = name
        self.obj = obj
        self.removed = None
        
    def do(self, scope):
        try:
            self.removed = getattr(scope, self.name)
        except AttributeError:
            pass
        scope.add(self.name, self.obj)
        
    def undo(self, scope):
        scope.remove(self.name)
        if self.removed is not None:
            scope.add(self.name, self.removed)
        
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
        
class SetAction(ContainerAction):
    def __init__(self, name, value):
        self.name = name
        self.value = copy.copy(value)
    
    def do(self, scope):
        scope.disconnect(self.src, self.dest)
        
    def undo(self, scope):
        scope.connect(self.src, self.dest)
        

class ActionManager(object):
    def __init__(self):
        self._stack = []
        
    def do(self, action, scope):
        """Do the given action and store it for later undoing"""
        action.do(scope)
        self._stack.append(action)
        
    def undo(self, scope):
        """Undo all of the actions."""
        while self._stack:
            action = self._stack.pop()
            action.undo(scope)

    