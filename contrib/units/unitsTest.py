from units import PhysicalQuantity as pq2
from Scientific.Physics.PhysicalQuantities import PhysicalQuantity as pq1

scientific_unit = pq1("1 1/min")
justin_unit = pq2("1 1/min")

print scientific_unit.unit.factor, scientific_unit.unit.names, scientific_unit.unit.powers

print justin_unit.unit.factor,justin_unit.unit.names,justin_unit.unit.powers