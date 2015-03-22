""" Finite Difference helper objects.
"""

# pylint: disable=E0611,F0401
from sys import float_info

from openmdao.main.array_helpers import flattened_size
from openmdao.main.interfaces import IVariableTree
from openmdao.main.mp_support import has_interface
from openmdao.main.mpiwrap import MPI
from openmdao.util.graph import base_var

from numpy import ndarray, zeros, ones, unravel_index, complex128


class FiniteDifference(object):
    """ Helper object for performing finite difference on a portion of a model.
    """

    def __init__(self, system, inputs, outputs, return_format='array'):
        """ Performs finite difference on the components in a given
        System. """

        self.inputs = inputs
        self.outputs = outputs
        self.in_bounds = {}
        self.system = system
        self.scope = system.scope
        self.return_format = return_format

        options = system.options
        driver = options.parent

        self.fd_step = options.fd_step*ones((len(self.inputs)))
        self.low = [None] * len(self.inputs)
        self.high = [None] * len(self.inputs)

        self.form = options.fd_form
        self.form_custom = {}
        self.step_type = options.fd_step_type
        self.step_type_custom = {}
        self.relative_threshold = 1.0e-4

        dgraph = self.scope._depgraph
        driver_params = []
        driver_targets = []

        if hasattr(driver, 'get_parameters'):
            driver_params = driver.get_parameters()
            driver_targets = driver.list_param_targets()

        in_size = 0
        for j, srcs in enumerate(self.inputs):

            low = high = None

            # Support for parameter groups
            if isinstance(srcs, basestring):
                srcs = [srcs]

            # Local stepsize support
            meta = self.scope.get_metadata(base_var(dgraph, srcs[0]))

            if 'fd_step' in meta:
                self.fd_step[j] = meta['fd_step']

            if 'low' in meta:
                low = meta['low']
            if 'high' in meta:
                high = meta['high']

            # Settings in the add_parameter call trump all others
            param_srcs = [item for item in srcs if item in driver_targets]
            if param_srcs:
                if param_srcs[0] in driver_params:
                    param = driver_params[param_srcs[0]]
                    if param.fd_step is not None:
                        self.fd_step[j] = param.fd_step
                    if param.low is not None:
                        low = param.low
                    if param.high is not None:
                        high = param.high
                else:
                    # have to check through all the param groups
                    for param_group in driver_params:
                        is_fd_step_not_set = is_low_not_set = \
                                             is_high_not_set = True
                        if not isinstance(param_group, str) and \
                           param_srcs[0] in param_group:
                            param = driver_params[param_group]
                            if is_fd_step_not_set and param.fd_step is not None:
                                self.fd_step[j] = param.fd_step
                                is_fd_step_not_set = False
                            if is_low_not_set and param.low is not None:
                                low = param.low
                                is_low_not_set = False
                            if is_high_not_set and param.high is not None:
                                high = param.high
                                is_high_not_set = False

            if 'fd_step_type' in meta:
                self.step_type_custom[j] = meta['fd_step_type']
                step_type = self.step_type_custom[j]
            else:
                step_type = self.step_type

            # Bounds scaled
            if step_type == 'bounds_scaled':
                if low is None and high is None:
                    raise RuntimeError("For variable '%s', a finite "
                                       "difference step type of bounds_scaled "
                                       "is used but required low and "
                                       "high values are not set" % srcs[0])
                if low == - float_info.max:
                    raise RuntimeError("For variable '%s', a finite "
                                       "difference step type of "
                                       "bounds_scaled is used but required "
                                       "low value is not set" % srcs[0])
                if high == float_info.max:
                    raise RuntimeError("For variable '%s', a finite "
                                       "difference step type of "
                                       "bounds_scaled is used but required "
                                       "high value is not set" % srcs[0])
                self.fd_step[j] = (high - low) * self.fd_step[j]

            if 'fd_form' in meta:
                self.form_custom[j] = meta['fd_form']

            val = self.scope.get(srcs[0])
            width = flattened_size(srcs[0], val, self.scope)

            for src in srcs:
                self.in_bounds[src] = (in_size, in_size+width)
            in_size += width

            self.high[j] = high
            self.low[j] = low

        out_size = 0
        for src in self.outputs:
            val = self.scope.get(src)
            width = flattened_size(src, val)
            out_size += width

        # Size our Jacobian
        if return_format == 'dict':
            self.J = {}
            for okey in outputs:

                self.J[okey] = {}
                for ikey in inputs:
                    if isinstance(ikey, tuple):
                        ikey = ikey[0]

                    # If output not on this process, just allocate a dummy
                    # array
                    if MPI and okey not in self.system.vec['u']:
                        osize = 0
                    else:
                        osize = self.system.vec['u'][okey].size

                    isize = self.system.vec['p'][ikey].size

                    self.J[okey][ikey] = zeros((osize, isize))
        else:
            self.J = zeros((out_size, in_size))

        self.y_base = zeros((out_size,))
        self.y = zeros((out_size,))
        self.y2 = zeros((out_size,))

    def solve(self, iterbase=''):
        """Return Jacobian for all inputs and outputs."""

        iterbase = 'fd-' + iterbase
        uvec = self.system.vec['u']

        # For MPI, don't compute outputs that aren't on our processor.
        if MPI:
            outputs = [out for out in self.outputs
                       if out in self.system.vec['u']]
        else:
            outputs = self.outputs

        uvec.set_to_array(self.y_base, outputs)

        for j, src, in enumerate(self.inputs):

            # Users can customize relative/absolute step type per variable.
            if j in self.step_type_custom:
                step_type = self.step_type_custom[j]
            else:
                step_type = self.step_type

            if isinstance(src, basestring):
                i1, i2 = self.in_bounds[src]
            else:
                i1, i2 = self.in_bounds[src[0]]

            for i in range(i1, i2):

                # Users can customize the FD form per variable. Need to reset
                # it for each array element so that we can do bounds
                # detection.
                if j in self.form_custom:
                    form = self.form_custom[j]
                else:
                    form = self.form

                # Relative stepsizing
                fd_step = self.fd_step[j]
                current_val = self.get_value(src, i1, i2, i)
                if step_type == 'relative':
                    if current_val > self.relative_threshold:
                        fd_step = fd_step*current_val

                # Switch to forward if we get near the low boundary
                if self.low[j] is not None:
                    if isinstance(self.low[j], (list, ndarray)):
                        bound_val = self.low[j][i]
                    else:
                        bound_val = self.low[j]
                    if current_val - fd_step < bound_val:
                        form = 'forward'

                # Switch to backward if we get near the high boundary
                if self.high[j] is not None:
                    if isinstance(self.high[j], (list, ndarray)):
                        bound_val = self.high[j][i]
                    else:
                        bound_val = self.high[j]
                    if current_val + fd_step > bound_val:
                        form = 'backward'

                #--------------------
                # Forward difference
                #--------------------
                if form == 'forward':

                    # Step
                    self.set_value(src, fd_step, i-i1)

                    self.system.run(iterbase)
                    self.get_outputs(self.y, outputs)

                    # Forward difference
                    Jfd = (self.y - self.y_base)/fd_step

                    # Undo step
                    self.set_value(src, -fd_step, i-i1)

                #--------------------
                # Backward difference
                #--------------------
                elif form == 'backward':

                    # Step
                    self.set_value(src, -fd_step, i-i1)

                    self.system.run(iterbase)
                    self.get_outputs(self.y, outputs)

                    # Backward difference
                    Jfd = (self.y_base - self.y)/fd_step

                    # Undo step
                    self.set_value(src, fd_step, i-i1)

                #--------------------
                # Central difference
                #--------------------
                elif form == 'central':

                    # Forward Step
                    self.set_value(src, fd_step, i-i1)

                    self.system.run(iterbase)
                    self.get_outputs(self.y, outputs)

                    # Backward Step
                    self.set_value(src, -2.0*fd_step, i-i1)

                    self.system.run(iterbase)
                    self.get_outputs(self.y2, outputs)

                    # Central difference
                    Jfd = (self.y - self.y2)/(2.0*fd_step)

                    # Undo step
                    self.set_value(src, fd_step, i-i1)

                #--------------------
                # Complex Step
                #--------------------
                elif form == 'complex_step':

                    complex_step = fd_step
                    yc = zeros(len(self.y), dtype=complex128)
                    self.system.set_complex_step(True)

                    # Step
                    self.set_value_complex(src, complex_step, i-i1)

                    self.system.run(iterbase)
                    self.get_complex_outputs(yc)

                    # Forward difference
                    Jfd = (yc/fd_step).imag

                    # Undo step
                    self.set_value_complex(src, complex_step, i-i1,
                                           undo_complex=True)
                    self.system.set_complex_step(False)

                # Pack Jacobian in either an array or a dictionary.
                if self.return_format == 'dict':
                    start = end = 0
                    for okey in outputs:

                        sz = uvec[okey].size
                        end += sz
                        #print Jfd, start, end, i, self.J
                        self.J[okey][src][:, i-i1] = Jfd[start:end]
                        start += sz
                else:
                    self.J[:, i] = Jfd

        # Restore final inputs/outputs.
        uvec.set_from_array(self.y_base, outputs)
        uvec.set_to_scope(self.scope)

        #print 'after FD', self.J
        return self.J

    def get_outputs(self, x, outputs):
        """Return matrix of flattened values from output edges."""

        uvec = self.system.vec['u']
        start = end = 0

        for src in outputs:
            uarray = uvec[src]
            end += uarray.size
            x[start:end] = uarray
            start += uarray.size

    def get_complex_outputs(self, x):
        """Return matrix of flattened values from output edges.
        This version is used by complex step."""

        uvec = self.system.vec['u']
        duvec = self.system.vec['du']
        start = end = 0

        for src in self.outputs:
            uarray = uvec[src]
            end += uarray.size
            x[start:end] = uarray + duvec[src]*1j
            start += uarray.size

    def set_value(self, srcs, val, index):
        """Set a value in the model"""

        # Support for Parameter Groups:
        if isinstance(srcs, basestring):
            srcs = [srcs]

        uvec = self.system.vec['u']
        for src in srcs:
            if src in uvec:
                uvec[src][index] += val

                # avoid adding to the same array entry multiple times for
                # param groups
                break

        if self.system.name.split('.')[-1] == '_inner_asm':
            uvec.set_to_scope(self.system.scope, vnames=srcs)

    def set_value_complex(self, srcs, val, index, undo_complex=False):
        """Set/unset a complex value in the model"""

        # Support for Parameter Groups:
        if isinstance(srcs, basestring):
            srcs = [srcs]

        for src in srcs:
            du = self.system.vec['du']
            if src in du:
                if undo_complex is True:
                    du[src][index] = 0.0
                else:
                    du[src][index] = val

                # avoid adding to the same array entry multiple times for
                # param groups
                break

    def get_value(self, src, i1, i2, index):
        """Get a value from the model. We only need this function for
        determining the relative stepsize to take."""

        # Parameters groups all have same value, so only take from
        # first one.
        if not isinstance(src, basestring):
            src = src[0]

        old_val = self.scope.get(src)

        # Full vector
        if i2-i1 > 1:
            index = index - i1

            # Indexed array slice
            if '[' in src:
                flattened_src = old_val.flatten()
                old_val = flattened_src[index]
            else:
                unravelled = unravel_index(index, old_val.shape)
                old_val = old_val[unravelled]

        return old_val


class DirectionalFD(object):
    """ Helper object for performing a finite difference in a single direction.
    """

    def __init__(self, system, inputs, outputs):
        """ Performs finite difference on the components in a given
        pseudo_assembly. """

        self.inputs = inputs
        self.outputs = outputs
        self.in_bounds = {}

        self.system = system
        self.scope = system.scope

        in_size = 0
        for srcs in self.inputs:

            # Support for parameter groups
            if isinstance(srcs, basestring):
                srcs = [srcs]

            val = self.scope.get(srcs[0])
            width = flattened_size(srcs[0], val, self.scope)

            for src in srcs:
                self.in_bounds[src] = (in_size, in_size+width)
            in_size += width

        out_size = 0
        for src in self.outputs:
            val = self.scope.get(src)
            width = flattened_size(src, val)
            out_size += width

        self.y_base = zeros((out_size,))
        self.y = zeros((out_size,))
        self.y2 = zeros((out_size,))

    def calculate(self, arg, result, iterbase=''):
        """Return Jacobian of all outputs with respect to a given direction in
        the input space."""

        iterbase = 'fd-' + iterbase

        self.system.vec['u'].set_to_array(self.y_base,
                                          self.outputs)

        options = self.system.options
        fd_step = options.fd_step
        form = options.fd_form

        #--------------------
        # Forward difference
        #--------------------
        if form == 'forward':

            # Step
            self.set_value(fd_step, arg)

            self.system.run(iterbase)
            self.get_outputs(self.y)

            # Forward difference
            mv_prod = (self.y - self.y_base)/fd_step

            # Undo step
            self.set_value(-fd_step, arg)

        #--------------------
        # Backward difference
        #--------------------
        elif form == 'backward':

            # Step
            self.set_value(-fd_step, arg)

            self.system.run(iterbase)
            self.get_outputs(self.y)

            # Backward difference
            mv_prod = (self.y_base - self.y)/fd_step

            # Undo step
            self.set_value(fd_step, arg)

        #--------------------
        # Central difference
        #--------------------
        elif form == 'central':

            # Forward Step
            self.set_value(fd_step, arg)

            self.system.run(iterbase)
            self.get_outputs(self.y)

            # Backward Step
            self.set_value(-2.0*fd_step, arg)

            self.system.run(iterbase)
            self.get_outputs(self.y2)

            # Central difference
            mv_prod = (self.y - self.y2)/(2.0*fd_step)

            # Undo step
            self.set_value(fd_step, arg)

        #--------------------
        # Complex Step
        #--------------------
        elif form == 'complex_step':

            yc = zeros(len(self.y), dtype=complex128)
            self.system.set_complex_step(True)

            # Step
            self.set_value_complex(fd_step, arg)

            self.system.run(iterbase)
            self.get_outputs_complex(yc)

            # Forward difference
            mv_prod = (yc/fd_step).imag

            # Undo step
            self.set_value_complex(fd_step, arg, undo_complex=True)
            self.system.set_complex_step(False)

        # Pack the results dictionary
        j = 0
        for key in self.outputs:
            indices = self.system.vec['u'].indices(self.system, key)
            i1, i2 = j, j+len(indices)

            old_val = self.scope.get(key)

            if isinstance(old_val, (float, complex)):
                result[key] += mv_prod[i1:i2]
            elif isinstance(old_val, ndarray):
                shape = old_val.shape
                if len(shape) > 1:
                    result[key] += mv_prod[i1:i2].reshape(shape)
                else:
                    result[key] += mv_prod[i1:i2]
            elif has_interface(old_val, IVariableTree):
                result[key] += mv_prod[i1:i2]
            else:
                continue

            j += len(indices)

        # Restore final inputs/outputs.
        self.system.vec['u'].set_from_array(self.y_base, self.outputs)
        self.system.vec['u'].set_to_scope(self.scope)

        #print 'after DFD', mv_prod, arg, result

    def set_value(self, fd_step, arg):
        """Set a value in the model"""

        for srcs in self.inputs:

            # Support for Parameter Groups:
            if isinstance(srcs, basestring):
                srcs = (srcs,)

            direction = fd_step*arg[srcs[0]].flatten()

            uvec = self.system.vec['u']
            for src in srcs:
                if src in uvec:
                    uvec[src] += direction

    def set_value_complex(self, fd_step, arg, undo_complex=False):
        """Set a complex value in the model"""

        for srcs in self.inputs:

            # Support for Parameter Groups:
            if isinstance(srcs, basestring):
                srcs = (srcs,)

            direction = fd_step*arg[srcs[0]].flatten()

            du = self.system.vec['du']
            for src in srcs:
                if src in du:
                    if undo_complex is True:
                        du[src][:] = 0.0
                    else:
                        du[src][:] = direction

    def get_outputs(self, x):
        """Return matrix of flattened values from output edges."""

        uvec = self.system.vec['u']
        start = end = 0

        for src in self.outputs:
            uarray = uvec[src]
            end += uarray.size
            x[start:end] = uarray
            start += uarray.size

    def get_outputs_complex(self, x):
        """Return matrix of flattened values from output edges.
        For complex step."""

        uvec = self.system.vec['u']
        duvec = self.system.vec['du']
        start = end = 0

        for src in self.outputs:
            uarray = uvec[src]
            end += uarray.size
            x[start:end] = uarray + 1j*duvec[src]
            start += uarray.size
