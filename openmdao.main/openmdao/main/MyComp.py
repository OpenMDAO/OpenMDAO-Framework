
from openmdao.main.component import Component

class MyComp(Component):
   def __init__(self, name=None, parent=None):
      Component.__init__(self, name, parent)

   def execute(self):
      print 'hello from MyComp'


