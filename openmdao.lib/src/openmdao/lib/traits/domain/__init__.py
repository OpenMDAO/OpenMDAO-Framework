"""
The `domain` package provides a data structure for representing multi-zone
meshes and their related data. The goal is to be able to represent all
concepts from CGNS, though not necessarily in the same way.

It also provides functions for reading and writing in various formats
(currently just Plot3D).
"""

from domain import DomainObj
from vector import Vector
from zone   import Zone
from plot3d import read_plot3d_q, read_plot3d_f, read_plot3d_grid, \
                   write_plot3d_q, write_plot3d_f, write_plot3d_grid

