# testpdcyl.py

#   program tests python module pdcylm...
#   input  file:  PDCYL.in
#   output file: PDCYL.out

import sys
import numpy
import subprocess
import shutil
import os

if '.' not in sys.path:
    sys.path.append('.')



import  pdcylm 

print pdcylm.__doc__
print pdcylm.pdcylm.__doc__
loop = 1
while  loop < 2 :
    print 'loop =',loop
    n = loop % 2
    one = pdcylm.pdcylm()
    print 'loop =',loop,'  After PDCYL in testpdcyl****'
    loop = loop + 1

print ' COMPLETE ********'
print dir('pdcylm')

