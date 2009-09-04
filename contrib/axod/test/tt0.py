# testaxod.py

import sys
import numpy
import subprocess
import shutil
import os

if '.' not in sys.path:
    sys.path.append('.')


import axod 

print axod.__doc__
print axod.axod.__doc__

print dir('axod')
loop = 1


