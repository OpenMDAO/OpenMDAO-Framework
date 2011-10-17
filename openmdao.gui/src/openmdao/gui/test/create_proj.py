from openmdao.main.component import Component
from openmdao.lib.datatypes.api import Float
from openmdao.main.project import *

multiplier_def = """
from openmdao.main.component import Component
from openmdao.lib.datatypes.api import Float

class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')
    
    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 6.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult
"""
        
def fill_project(top):
    ''' this is the example from openmdao.main.test.test_project
    '''
    f = open('multiplier.py','w')
    f.write(multiplier_def)
    f.close()
    
    from multiplier import Multiplier
    comp1 = top.add('comp1', Multiplier())
    comp2 = top.add('comp2', Multiplier())
    
    top.driver.workflow.add(['comp1', 'comp2'])
    
    top.comp1.mult = 2.0
    top.comp2.mult = 4.0
    top.connect('comp1.rval_out', 'comp2.rval_in')
    top.comp1.rval_in = 5.0

if __name__ == "__main__":
    save_dir = os.getcwd();
    proj = Project('foo') # creating the project will chdir into the proj dir
    fill_project(proj.top) 
    os.chdir(save_dir)  
    proj.export(projname='Project_Foo')
