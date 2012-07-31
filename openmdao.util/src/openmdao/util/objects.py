
from sys import modules

def has_class_changed(klass):
    """Return True if the given class is different from the class of the same name
    in the module that the class came from.  This happens if the module was
    reloaded after the given class object was created.
    """
    try:
        return id(klass) != id(getattr(modules[klass.__module__], klass.__name__))
    except:
        return False
    
    