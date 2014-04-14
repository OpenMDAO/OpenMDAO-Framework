"""
The AdaptiveSampleDriver behaves like a DOEDriver for the first iteration
and a CaseIteratorDriver thereafter.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.drivers.doedriver import DOEdriver

class AdaptiveSampleDriver(DOEdriver):

    def __init__(self):
        super(AdaptiveSampleDriver, self).__init__()

        self.run_DOE = True

    def config_changed(self, update_parent=True):
        """Reset so we run the DOE again. """
        super(AdaptiveSampleDriver, self).config_changed(update_parent)
        self.run_DOE = True

    def execute(self):
        """First time through, run points from our DOE. After that, take
        in and run points from our case iterator inputs.
        """

        # DOE
        if self.run_DOE is True:
            print 'run doe'
            super(AdaptiveSampleDriver, self).execute()
            self.run_DOE = False

            # Clean out the DOE inputs to prepare for case inputs
            for path in self.get_parameters():
                if isinstance(path, tuple):
                    path = path[0]  # Use first target of ParameterGroup.
                var_name = 'case_inputs.' + path
                val = self.get(var_name)
                if len(val) > 1:
                    # Use the last point we ran as the placeholder
                    self.set(var_name, [val[-1]])

        # Case Iterator
        else:
            print 'run cid', self.case_inputs.driven.x
            super(DOEdriver, self).execute()

    def add_parameter(self, target, low=None, high=None,
                      scaler=None, adder=None, start=None,
                      fd_step=None, name=None, scope=None):
        """We need to put dummy cases in so that we can connect a FPI."""

        super(AdaptiveSampleDriver, self).add_parameter(
                  target, low, high, scaler, adder, start, fd_step, name, scope)

        # Parameter gets [0] as an initial case, just for connectability.
        if isinstance(target, tuple):
            target = target[0]  # Use first target of ParameterGroup.
        var_name = 'case_inputs.' + target
        self.set(var_name, [0.0])
