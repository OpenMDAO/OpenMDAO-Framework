
import threading

from openmdao.main.api import Component
from openmdao.main.interfaces import IGeometryEngine
from openmdao.main.datatypes.api import Slot


class GeometryComp(Component): 
    """A component that represents a 3D geometry.
    """
    geom_engine = Slot(IGeometryEngine)
    
        
    def execute(self): 
        pass
        

