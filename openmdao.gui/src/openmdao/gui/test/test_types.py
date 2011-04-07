from openmdao.main.factorymanager import get_available_types
#from openmdao.gui.mdao_util import print_list

# utility function to print the contents of a list
def print_list (list):
    for item in list:
        print item
        
print 'All types:'
t =get_available_types()
print_list(t)

types = [x[0] for x in get_available_types()]
print types

g = [ 'openmdao.component' ]
print 'Components:'
t =get_available_types(groups=g)
print_list(t)

g = [ 'openmdao.driver' ]
print 'Drivers:'
t =get_available_types(groups=g)
print_list(t)

g = [ 'openmdao.workflow' ]
print 'Workflows:'
t =get_available_types(groups=g)
print_list(t)

