from openmdao.main.api import VariableTree
from openmdao.main.datatypes.api import Float

class Stream(VariableTree):
    """Container of variables defining the properties of a single stream"""

    Pt = Float(units='lbf/ft**2', desc='Stream total pressure')
    Tt = Float(units='degR', desc='Stream total temperature')
    Mach = Float(desc='Stream Mach number')
    Vel = Float(units='ft/s', desc='Stream fully expanded velocity')
    W = Float(units='lbm/s', desc='Stream mass flow rate')
