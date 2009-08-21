
__all__ = ["ObjServerFactory"]


from openmdao.main.api import Factory
   
class ObjServerFactory(Factory):

    def __init__(self):
        super(ObjServerFactory, self).__init__()

    def create(self, typname, name='', version=None, server=None, 
               res_desc=None, **ctor_args):
        pass

    
