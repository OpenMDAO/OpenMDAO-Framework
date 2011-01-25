
from inspect import getsourcefile

def get_classname(instance):
    return instance.__class__.__name__
    
class ConfigInfo(object):
    def __init__(self, instance, name, *initargs, **initkwargs):
        self.name = name  # name of the object that this config describes
        self.classname = get_classname(instance)
        self.initargs = initargs
        self.initkwargs = initkwargs
        # the following is a list of instructions that can have 2 possible forms:
        #   1) a string (python syntax with possible %(pathname)s formatting in it)
        #   2) a ConfigInfo object for a child who is to be initialized
        self.cmds = []
    
    def save_as_class(self, stream):
        lines = []
        
        stream.write('\n'.join(lines))
        