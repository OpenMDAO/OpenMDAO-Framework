import units
#from units import _addUnit
#import Scientific.Physics.PhysicalQuantities as units




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

