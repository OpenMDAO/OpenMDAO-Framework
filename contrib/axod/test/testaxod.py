import shutil
import sys

sys.path.append('../src')
import axod

print axod.__doc__
print axod.axod.__doc__
print dir('axod')

loop = 1
while  loop < 10:
    if loop % 2:
        input = 'eee_hpt.inp'
    else:
        input = 'one_stage.inp'
    print 'Running AXOD with', input
    shutil.copy(input, 'axod.inp')

    axod.axod()

    axod1 = 'axod.out'+ str(loop)
    shutil.copy('axod.out', input+'.out%d' % loop)
    loop = loop + 1

