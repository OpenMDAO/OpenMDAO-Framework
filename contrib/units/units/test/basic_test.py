from units import PhysicalQuantity as pq

x = pq("1cm")
y = pq("1s")

z = x/y

print x.unit.powers
print y.unit.powers
print z.unit.powers

print x.unit.names
print y.unit.names
print z.unit.names
