from openmdao.main.api import VariableTree
from openmdao.main.datatypes.api import Float, VarTree
from stream import Stream

class MEflows(VariableTree):
    """Container of variables defining the flow properties of the mixer-ejector nozzle"""

    gamma = Float(1.4, desc='Ratio of specific heats')
    Pstatic = Float(2116.8, units='lbf/ft**2', desc='Freestream static pressure')
    #alt = Float(0.0, units='ft', desc='Altitude')
    #Mach = Float(0.0, desc='Freestream Mach number')

    pri = VarTree(Stream())
    sec = VarTree(Stream())

