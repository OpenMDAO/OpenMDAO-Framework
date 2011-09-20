
from openmdao.main.container import Container


class VariableTree(Container):
    
    def __init__(self, iotype='out', doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self.iotype = iotype
                        
    def get_iotype(self, name):
        t = self.get_trait(name)
        if t is None:
            self.raise_exception("'%s' not found" % name)
        return self.iotype
    
    #def _add_path(self, msg):
        #"""Adds our pathname to the beginning of the given message"""
        #return "%s: %s" % (self.get_pathname(), msg)
