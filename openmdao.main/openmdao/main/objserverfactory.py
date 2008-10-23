

#
# programmer comments here, copyright, etc.
#


#public symbols
__all__ = ["ObjServerFactory"]

__version__ = "0.1"


import sys

from openmdao.main.factory import Factory
   
class ObjServerFactory(Factory):

    def __init__(self):
        pass

    def create(self, typname, name=None, version=None, server=None, 
               res_desc=None):
        pass

    