# testaxodn.py

import sys
import numpy
import subprocess
import shutil
import os

if '.' not in sys.path:
    sys.path.append('.')


from axod_compn import AxodCompn
from openmdao.main.api import Component, set_as_top, Container

#    changing data in input file ............................

one = set_as_top(AxodCompn(input_filename='eee_hpt.inp'))
one.Case1.Stage1.ptin = 51.00
one.Case1.Stage1.ttin = 1284.0
#one.Case1.Stage1.vctd = 1.0
#one.Case1.Stage1.rwg = [1.0,1.0895,1.105,1.1759,1.1760]
#one.Case1.Stage2.rwg = [1.1759,1.2007,1.2008,1.2009,1.2007]
one.Case1.Stage1.seta = [0.953,0.954,0.953,0.954,0.950]
one.Case1.Stage1.rpm = 8000.0
#one.Case3.Stage1.rpm = 4900.0
one.run()

print 'one.hpower=',one.hpower
print ' COMPLETE ********'


