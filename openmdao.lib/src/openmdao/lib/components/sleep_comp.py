"""
    sleep_comp.py - component that does one thing and does it
                     well. Sleep. 

                     Useful for slowing down a simulation to see
                     what is happening
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

import time

class SleepComp(Component):
    """Sleep for a given number of secons"""
    
    # pylint: disable-msg=E1101
    sleep_time = Float(0.0, iotype='in', desc='The number of seconds to sleep')

    def __init__(self):
        super(SleepComp, self).__init__()
        
    def execute(self):

        print "in execute of SleepComp with self.sleep_time = ", self.sleep_time

        time.sleep( self.sleep_time )
