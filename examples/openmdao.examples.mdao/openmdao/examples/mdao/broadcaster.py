"""
A simple component that pushes its input to its outputs. This is needed so
that an optimizer can set a design variable in one framework location, and
it can be broadcast to multiple other locations (including those that are
in expressions like Constraints or Objectives.)
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

class Broadcaster(Component):
    """Component that holds some design variables.
    This is only needed because we can't hook an optimizer up to multiple
    locations of the same design variable"""
    
    # pylint: disable-msg=E1101
    z1_in = Float(0.0, iotype='in', desc='Global Design Variable')
    z2_in = Float(0.0, iotype='in', desc='Global Design Variable')
    x1_in = Float(0.0, iotype='in', desc='Local Design Variable for CO')
    y1_in = Float(0.0, iotype='in', desc='Coupling Variable')
    y2_in = Float(0.0, iotype='in', desc='Coupling Variable')
    z1 = Float(0.0, iotype='out', desc='Global Design Variable')
    z2 = Float(0.0, iotype='out', desc='Global Design Variable')
    x1 = Float(0.0, iotype='out', desc='Local Design Variable for CO')
    y1 = Float(0.0, iotype='out', desc='Coupling Variable')
    y2 = Float(0.0, iotype='out', desc='Coupling Variable')
    
    def execute(self):
        """ Pass everything through"""
        self.z1 = self.z1_in
        self.z2 = self.z2_in
        self.x1 = self.x1_in
        self.y1 = self.y1_in
        self.y2 = self.y2_in