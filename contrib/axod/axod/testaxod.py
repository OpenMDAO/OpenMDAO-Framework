# testaxod.py

import sys
import numpy
import subprocess
import shutil
import os

if '.' not in sys.path:
    sys.path.append('.')


import axodm 

print axodm.__doc__
print axodm.axod.__doc__
loop = 1
while  loop < 10:
    print 'loop =',loop
    n = loop % 2
    if n == 0: shutil.copy('hpt.inp','axod.inp')
    if n == 1: shutil.copy('lpt.inp','axod.inp')
    axodm.axod(n)
    print 'loop =',loop,'  After AXOD  in testaxod.py****'
    axod1 = 'axod.out'+ str(loop)
    shutil.copy('axod.out',axod1)
    loop = loop + 1

print ' COMPLETE ********'
print dir('axodm')


