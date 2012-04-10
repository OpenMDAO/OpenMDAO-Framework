import unittest
import sys

# does not work with nose

from functools import partial
class TestAllReports(unittest.TestCase):
    pass

def getSample( name ):
    return name

def classify(sample ):
    return 1
    
def test_spamreport(name):
    assert classify(getSample(name))=='spamreport', name

REPORTS = [ 'eeee', 'wwwwe' ]

for rep in REPORTS:
    testname = 'test_'+rep
    testfunc = partial(test_spamreport, rep)
    testfunc.__doc__ = testname
    setattr( TestAllReports, testname, testfunc )

if __name__=='__main__':
    unittest.main(argv=sys.argv + ['--verbose'])


