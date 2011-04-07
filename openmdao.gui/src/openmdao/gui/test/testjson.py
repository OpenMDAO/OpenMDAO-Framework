import jsonpickle
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

fileObj = open("testjson.json","w")
fileObj.write(json)
fileObj.close()


