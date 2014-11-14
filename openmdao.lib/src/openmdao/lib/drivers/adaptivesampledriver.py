"""
The AdaptiveSampleDriver behaves like a DOEDriver for the first iteration
and a CaseIteratorDriver thereafter.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.main.datatypes.api import List, VarTree
from openmdao.main.pseudocomp import _remove_spaces
from openmdao.main.variable import make_legal_path
from openmdao.main.vartree import VariableTree


class AdaptiveSampleDriver(DOEdriver):

    # pylint: disable-msg=E1101
    
    adaptive_inputs = VarTree(VariableTree(), iotype='in')

    all_case_inputs = VarTree(VariableTree(), iotype='out')
    all_case_outputs = VarTree(VariableTree(), iotype='out')

    # Save the DOE data seperately
    DOE_inputs = VarTree(VariableTree(), iotype='out')
    DOE_outputs = VarTree(VariableTree(), iotype='out')

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
            super(AdaptiveSampleDriver, self).execute()
            self.run_DOE = False
            self._accumulate_data(reset=True)

            # Workaround for bug in cyclic flow
            for path in self.get_parameters():
                if isinstance(path, tuple):
                    path = path[0]  # Use first target of ParameterGroup.
                src = 'adaptive_inputs.' + path
                target = 'case_inputs.' + path
                val = self.get(target)
                self.set(src, [val[-1]])

        # Case Iterator
        else:

            # Feed our new input to the parent
            changed = False
            for path in self.get_parameters():
                if isinstance(path, tuple):
                    path = path[0]  # Use first target of ParameterGroup.
                src = 'adaptive_inputs.' + path
                target = 'case_inputs.' + path
                src_val = self.get(src)
                target_val = self.get(target)
                if src_val != target_val[-1]:
                    changed = True
                    self.set(target, src_val)

            if changed:
                super(DOEdriver, self).execute()
                self._accumulate_data()

    def _accumulate_data(self, reset=False):
        ''' We save the original DOE data as well as every point we add
        afterwards'''

        for path in self.get_parameters():
            if isinstance(path, tuple):
                path = path[0]  # Use first target of ParameterGroup.

            # Accumulate the inputs
            src = 'case_inputs.' + path
            target = 'all_case_inputs.' + path
            src_val = self.get(src)
            if reset is True:
                target_val = []
                doe = 'DOE_inputs.' + path
                doe_val = []
                doe_val.extend(src_val)
                self.set(doe, doe_val)
            else:
                target_val = self.get(target)
            target_val.extend(src_val)
            self.set(target, target_val)

        for path in self.get_responses():

            # Accumulate the outputs
            src = 'case_outputs.' + path
            target = 'all_case_outputs.' + path
            src_val = self.get(src)
            if reset is True:
                target_val = []
                doe = 'DOE_outputs.' + path
                doe_val = []
                doe_val.extend(src_val)
                self.set(doe, doe_val)
            else:
                target_val = self.get(target)
            target_val.extend(src_val)
            self.set(target, target_val)


    def add_parameter(self, target, low=None, high=None,
                      scaler=None, adder=None, start=None,
                      fd_step=None, name=None, scope=None):
        """We need to create our special variable trees."""

        super(AdaptiveSampleDriver, self).add_parameter(
                  target, low, high, scaler, adder, start, fd_step, name, scope)

        # Parameter gets [0] as an initial case, just for connectability.
        if name is not None:
            target = name
        elif isinstance(target, tuple):
            target = target[0]  # Use first target of ParameterGroup.

        path = make_legal_path(target)

        desc = 'Input for adding new points during parent iteration.'
        self._add_vartrees('adaptive_inputs', path, desc)

        desc = 'Holds all inputs processed by this driver.'
        self._add_vartrees('all_case_inputs', path, desc)

        desc = 'Holds just the DOE inputs.'
        self._add_vartrees('DOE_inputs', path, desc)


    def add_response(self, expr, name=None, scope=None):
        """We need to create our special variable trees."""

        super(AdaptiveSampleDriver, self).add_response(expr, name, scope)

        path = _remove_spaces(expr) if name is None else name
        path = make_legal_path(path)

        desc = 'Holds all outputs processed by this driver.'
        self._add_vartrees('all_case_outputs', path, desc)

        desc = 'Holds just the DOE outputs.'
        self._add_vartrees('DOE_outputs', path, desc)


    def _add_vartrees(self, tree_name, path, desc):
        ''' Adds a vartree, the component sub leaf, and the final variable
        leaf.
        '''

        obj = self
        names = [tree_name] + path.split('.')
        for name in names[:-1]:
            if obj.get_trait(name):
                val = obj.get(name)
            else:
                val = VariableTree()
                obj.add_trait(name, VarTree(val, iotype='in'))
            obj = val

        name = names[-1]
        obj.add_trait(name, List([0.0], desc=desc))


