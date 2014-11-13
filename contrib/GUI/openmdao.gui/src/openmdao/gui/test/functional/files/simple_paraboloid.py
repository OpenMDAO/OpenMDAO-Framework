from openmdao.main.api import Assembly
from openmdao.examples.simple.paraboloid import Paraboloid

class SimpleParaboloid(Assembly):
    """ Top level assembly """

    def configure(self):
        """ Creates a new Assembly containing a chain of Paraboloid components"""

        self.add("paraboloid", Paraboloid())
        #shortcut syntax
        #self.connect("par1.f_xy", ["par2.x", "par3.y"])
