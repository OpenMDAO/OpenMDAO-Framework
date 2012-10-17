from openmdao.main.api import Assembly
from openmdao.examples.simple.paraboloid import Paraboloid
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver

class Basic_Model(Assembly):
    """ Top level assembly """

    def configure(self):
        """ Creates a new Assembly containing a chain of Paraboloid components"""

        self.add("paraboloid", Paraboloid())
        self.add("optimizer", SLSQPdriver())
        
        self.optimizer.add_parameter("paraboloid.x", low='-50', high='50')
        self.optimizer.add_parameter("paraboloid.y", low='-50', high='50')

        self.optimizer.add_objective("paraboloid.f_xy")

        self.optimizer.workflow.add('paraboloid')

        #shortcut syntax
        #self.connect("par1.f_xy", ["par2.x", "par3.y"])
