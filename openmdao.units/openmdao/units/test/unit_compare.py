#Comparison between NASA units and Sicentific PhysicalQuantities in terms of speed

import Scientific.Physics.PhysicalQuantities as case1
import units as case2
import time

import cProfile

n = 20000

#x1 = case1.PhysicalQuantity('5cm')
#x2 = case2.PhysicalQuantity('5cm')


t1 = time.time()

for jj in xrange(n):
    pq = case1.PhysicalQuantity(5, 'mi/h')
    pq.convert_to_unit('m/s')
    pq2 = pq**2
    pq2 = pq**2
    pq2 = pq*pq
    pq2 = pq/pq
    pq2 = pq + pq
    pq2 = pq - pq
    pq2 = pq**2
    pq2.sqrt()
    
    pq2 = case1.PhysicalQuantity(4,'rad')
    pq2.sin()
    pq2.cos()
    
print "Scientific -> Elapsed time: ", time.time()-t1
print ""

t2 = time.time()

for jj in xrange(n):
    pq = case2.PhysicalQuantity(5, 'mi/h')
    pq.convert_to_unit('m/s')
    pq2 = pq**2
    pq2 = pq*pq
    pq2 = pq/pq
    pq2 = pq + pq
    pq2 = pq - pq
    pq2 = pq**2
    pq2.sqrt()
    
    pq2 = case2.PhysicalQuantity(4,'rad')
    pq2.sin()
    pq2.cos()    
    
print "Justin -> Elapsed time: ", time.time()-t2


stmt = """
for jj in xrange(n):
    pq = case1.PhysicalQuantity(5, 'mi/h')
    pq.convert_to_unit('m/s')
    pq2 = pq**2
    pq2 = pq**2
    pq2 = pq*pq
    pq2 = pq/pq
    pq2 = pq + pq
    pq2 = pq - pq
    pq2 = pq**2
    pq2.sqrt()
    
    pq2 = case1.PhysicalQuantity(4,'rad')
    pq2.sin()
    pq2.cos()
"""
cProfile.run(stmt)

stmt = """
for jj in xrange(n):
    pq = case2.PhysicalQuantity(5, 'mi/h')
    pq.convert_to_unit('m/s')
    pq2 = pq**2
    pq2 = pq*pq
    pq2 = pq/pq
    pq2 = pq + pq
    pq2 = pq - pq
    pq2 = pq**2
    pq2.sqrt()
    
    pq2 = case2.PhysicalQuantity(4,'rad')
    pq2.sin()
    pq2.cos()    
"""

cProfile.run(stmt)