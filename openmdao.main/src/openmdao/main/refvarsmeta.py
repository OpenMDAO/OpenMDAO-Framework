__all__ = ['RefVarsMeta']
__version__ = "0.1"

class RefVarsMeta(type):
    """This class takes RefVariables and RefVariableArrays that are
    declared in the class and creates a property for them to 
    make it more convenient for the component developer.
    """

    def __new__(meta, classname, bases, classDict):
        newClassDict = { '_ref_dict': {} }

        for name, item in classDict.items():
            if isinstance(item, RefVariable) or isinstance(item, RefVariableArray):
                newClassDict[name] = property(lambda parent : parent.getvar(name), 
                                              None, None, item.__doc__)
                
            else:
                newclassDict[name] = item
        return type.__new__(meta, classname, bases, newClassDict)

    
class WithRefVariables(object):
    __metaclass__ = RefVarsMeta
    
