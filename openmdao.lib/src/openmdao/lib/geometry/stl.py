
from openmdao.main.interfaces import IStaticGeometry, classImplements

from pyV3D.stl import STLGeometryObject, STLSender

classImplements(STLGeometryObject, IStaticGeometry)
