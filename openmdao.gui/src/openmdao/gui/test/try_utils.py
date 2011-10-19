import os, os.path

from xml.dom.minidom import Document
import json, jsonpickle

from openmdao.gui.mdao_util import *

#
# try the 2 methods of representing a file tree (XML & JSON).
#
def test_filetree():
    testpath = os.path.abspath('..')

    doc = Document()    
    doc.appendChild(makenode(doc,testpath))
    print doc.toprettyxml()
    
    print ''    
    print ''
    
    dict = filedict(testpath)
    jsondict = jsonpickle.encode(dict)    
    print 'json = ',json.dumps(jsondict,sort_keys=False,indent=4)

#
# try out get_available_types()
#
def test_types():
    from openmdao.main.factorymanager import get_available_types
    
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

#
# try out json encoding and pickling
#
def test_json():
    from openmdao.examples.enginedesign.engine import Engine
    my_engine = Engine()

    print 'RPM =',my_engine.RPM
    json = jsonpickle.encode(my_engine)
    print 'json = ',json

    my_engine.RPM = 4000

    print 'RPM =',my_engine.RPM
    json = jsonpickle.encode(my_engine)
    print 'json = ',json

    from openmdao.examples.enginedesign.vehicle import Vehicle
    my_car = Vehicle()
    my_car.velocity = 25.0
    my_car.current_gear = 3
    my_car.throttle = .5

    json = jsonpickle.encode(my_car)
    print 'json = ',json

    # json = jsonpickle(_globals)

    #fileObj = open("testjson.json","w")
    #fileObj.write(json)
    #fileObj.close()

if __name__ == "__main__":
    test_filetree()
    test_types()
    test_json()
    
