import units
#from units import _addUnit
#import Scientific.Physics.PhysicalQuantities as units
import StringIO

bad_ini_file = StringIO.StringIO()
bad_ini_file.write("""
[prefixes]
Y: 1.e24
Z: 1.e21
E: 1.e18
P: 1.e15
T: 1.e12
G: 1.e9
M: 1.e6
k: 1.e3
h: 1.e2
da: 1.e1
d: 1.e-1
c: 1.e-2
m: 1.e-3
u: 1.e-6
n: 1.e-9
p: 1.e-12
f: 1.e-15
a: 1.e-18
z: 1.e-21
y: 1.e-24

[base_units]
length: m
current: A
temperature: degK
amount: mol
luminous_intesity: cd
angle: rad
solid_angle: sr
money: $

[units]
degC: 1,degK,273.15,centigrade
degR: 0.555555556*degK,Rankine
degF: 1,degR,459.67,Fahrenheit
g: .001*kg,gram
Hz: 1/s,hertz
""")

units.importLibrary(bad_ini_file)

exit()

length1 = units.PhysicalQuantity(6,'mm')
length2 = units.PhysicalQuantity(7,'inch')
time1 = units.PhysicalQuantity(1,'hr')

#length3 =PhysicalQuantity('7 furlong')
print length1
print length2

print (length1+length2).inUnitsOf('ft')

# The speed of a Townsend when startled by a stalking Gray: 
units.addUnit("furlong","660*ft","furlong")
units.addUnit("fortnight","2*wk","fortnight")

test1 = units.PhysicalQuantity(1,'mi')
test2 = units.PhysicalQuantity(1,'hr')
print (test1/test2).inUnitsOf('m/s')

speed1 =  units.PhysicalQuantity(1,'mi/hr')
speed2 = units.PhysicalQuantity(10000,'furlong/fortnight')
print speed1.inUnitsOf("furlong/fortnight")
print speed2.inUnitsOf("furlong/fortnight")

print "Finished Unit Test"

