

import Scientific.Physics.PhysicalQuantities as case1
import units as case2
import time

import cProfile

n = 20000

#x1 = case1.PhysicalQuantity('5cm')
#x2 = case2.PhysicalQuantity('5cm')

stmt = """
for jj in xrange(n):
    pq = case1.PhysicalQuantity(5, 'mi/h')
    pq.convertToUnit('m/s')
"""
cProfile.run(stmt)

stmt = """
for jj in xrange(n):
    pq = case2.PhysicalQuantity(5, 'mi/h')
    pq.convertToUnit('m/s')
"""

cProfile.run(stmt)