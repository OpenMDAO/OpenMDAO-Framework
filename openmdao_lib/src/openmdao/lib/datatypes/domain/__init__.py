"""
The `domain` package provides a data structure for representing multi-zone
meshes and their related data. The goal is to be able to represent all
concepts from CGNS, though not necessarily in the same way.

It also provides functions for obtaining flow values across a mesh surface
and reading/writing in files various formats (currently just Plot3D).
"""

from domain  import DomainObj
from flow    import FlowSolution
from grid    import GridCoordinates
from vector  import Vector
from zone    import Zone

from metrics import get_metric, list_metrics
from probe   import mesh_probe
from plot3d  import read_plot3d_q, read_plot3d_f, read_plot3d_grid, \
                    write_plot3d_q, write_plot3d_f, write_plot3d_grid, \
                    read_plot3d_shape

