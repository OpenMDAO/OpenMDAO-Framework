
from inspect import getsourcefile

class ConfigInfo(object):
    # in order to make our new class declaration, what info do we need:
    #    1) names/packages of any classes we need to create inside of our __init__
    #          (this is necessary when an object is a class instance and it is not created
    #           as part of the __init__ of the enclosing object)
    #    2) names of attributes we must assign values to.  In some cases these
    #        values will be class instances that we must initialize
    #    3) connections
    #    4) addition of parameters/objectives (driver specific)
    def __init__(self):
        self.attribs = []  # list of name, value tuples
        self.classes = set() # classes that must be imported at the top level
        self.files = set()
        self.children = []  # list of ConfigInfo objects for children
        
    def add_class(self, instance):
        klass = instance.__class__
        try:
            fname = getsourcefile(klass)
        except TypeError:
            return  # it's a builtin class
        self.files.add(fname)
        self.classes.add(klass)

