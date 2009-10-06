from units import PhysicalQuantity as pq

x = pq("1cm")
y = pq("1s")

z = x/y

print x.unit
print y.unit
print z.unit