"""
This shows an example of creating an MDAO model directly using a python script
"""


import openmdao.main.factorymanager as factorymanager
from openmdao.main.assembly import Assembly
from openmdao.main.importfactory import ImportFactory

factorymanager.register_factory(ImportFactory())
   
top = Assembly('top') # top level Assembly
top.create('openmdao.main.cobyla.COBYLA', name = 'driver')

driver = top.get('driver')




top.run()
    
