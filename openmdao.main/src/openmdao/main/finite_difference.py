""" Finite Difference helper objects.
"""

# pylint: disable=E0611,F0401
from sys import float_info

from openmdao.main.array_helpers import flattened_size, flattened_value
from openmdao.main.interfaces import IVariableTree
from openmdao.main.mp_support import has_interface

from numpy import ndarray, zeros, ones, unravel_index, complex128


class FiniteDifference(object):
    """ Helper object for performing finite difference on a portion of a model.
    """

    def __init__(self, pa):
        """ Performs finite difference on the components in a given
        pseudo_assembly. """

        self.inputs = pa.inputs
        self.outputs = pa.outputs
        self.in_bounds = {}
        self.out_bounds = {}
        self.pa = pa
        self.scope = pa.wflow.scope

        options = pa.wflow._parent.gradient_options

        self.fd_step = options.fd_step*ones((len(self.inputs)))
        self.low = [None] * len(self.inputs)
        self.high = [None] * len(self.inputs)

        self.form = options.fd_form
        self.form_custom = {}
        self.step_type = options.fd_step_type
        self.step_type_custom = {}
        self.relative_threshold = 1.0e-4

        driver = self.pa.wflow._parent
        dgraph = self.scope._depgraph
        driver_params = []
        driver_targets = []

        if hasattr(driver, 'get_parameters'):
            driver_params = self.pa.wflow._parent.get_parameters()
            driver_targets = driver.list_param_targets()

        in_size = 0
        for j, srcs in enumerate(self.inputs):

            low = high = None

            # Support for parameter groups
            if isinstance(srcs, basestring):
                srcs = [srcs]

            # Local stepsize support
            meta = self.scope.get_metadata(dgraph.base_var(srcs[0]))

            if 'fd_step' in meta:
                self.fd_step[j] = meta['fd_step']

            if 'low' in meta:
                low = meta[ 'low' ]
            if 'high' in meta:
                high = meta[ 'high' ]

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
                        is_fd_step_not_set = is_low_not_set = is_high_not_set = True
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
                if low is None and high is None :
                    raise RuntimeError("For variable '%s', a finite "
                                       "difference step type of bounds_scaled "
                                       "is used but required low and "
                                       "high values are not set" % srcs[0] )
                if low == - float_info.max:
                    raise RuntimeError("For variable '%s', a finite "
                                       "difference step type of "
                                       "bounds_scaled is used but required "
                                       "low value is not set" % srcs[0] )
                if high == float_info.max:
                    raise RuntimeError("For variable '%s', a finite "
                                       "difference step type of "
                                       "bounds_scaled is used but required "
                                       "high value is not set" % srcs[0] )
                self.fd_step[j] = ( high - low ) * self.fd_step[j]

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
            self.out_bounds[src] = (out_size, out_size+width)
            out_size += width

        self.J = zeros((out_size, in_size))
        self.y_base = zeros((out_size,))
        self.y = zeros((out_size,))
        self.y2 = zeros((out_size,))

    def calculate(self):
        """Return Jacobian for all inputs and outputs."""
        self.get_outputs(self.y_base)

        for j, src, in enumerate(self.inputs):

            # Users can customize the FD per variable
            if j in self.form_custom:
                form = self.form_custom[j]
            else:
                form = self.form
            if j in self.step_type_custom:
                step_type = self.step_type_custom[j]
            else:
                step_type = self.step_type

            if isinstance(src, basestring):
                i1, i2 = self.in_bounds[src]
            else:
                i1, i2 = self.in_bounds[src[0]]

            for i in range(i1, i2):

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
                    self.set_value(src, fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Forward difference
                    self.J[:, i] = (self.y - self.y_base)/fd_step

                    # Undo step
                    self.set_value(src, -fd_step, i1, i2, i)

                #--------------------
                # Backward difference
                #--------------------
                elif form == 'backward':

                    # Step
                    self.set_value(src, -fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Backward difference
                    self.J[:, i] = (self.y_base - self.y)/fd_step

                    # Undo step
                    self.set_value(src, fd_step, i1, i2, i)

                #--------------------
                # Central difference
                #--------------------
                elif form == 'central':

                    # Forward Step
                    self.set_value(src, fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Backward Step
                    self.set_value(src, -2.0*fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y2)

                    # Central difference
                    self.J[:, i] = (self.y - self.y2)/(2.0*fd_step)

                    # Undo step
                    self.set_value(src, fd_step, i1, i2, i)

                #--------------------
                # Complex Step
                #--------------------
                elif form == 'complex_step':

                    complex_step = fd_step*1j
                    self.pa.set_complex_step()
                    yc = zeros(len(self.y), dtype=complex128)

                    # Step
                    self.set_value(src, complex_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(yc)

                    # Forward difference
                    self.J[:, i] = (yc/fd_step).imag

                    # Undo step
                    self.set_value(src, -fd_step, i1, i2, i, undo_complex=True)

        # Return outputs to a clean state.
        for src in self.outputs:
            i1, i2 = self.out_bounds[src]
            old_val = self.scope.get(src)

            if isinstance(old_val, (float, complex)):
                new_val = float(self.y_base[i1:i2])
            elif isinstance(old_val, ndarray):
                shape = old_val.shape
                if len(shape) > 1:
                    new_val = self.y_base[i1:i2]
                    new_val = new_val.reshape(shape)
                else:
                    new_val = self.y_base[i1:i2]
            elif has_interface(old_val, IVariableTree):
                new_val = old_val.copy()
                self.pa.wflow._update(src, new_val, self.y_base[i1:i2])
            else:
                continue

            src, _, idx = src.partition('[')
            if idx:
                old_val = self.scope.get(src)
                if isinstance(new_val, ndarray):
                    exec('old_val[%s = new_val.copy()' % idx)
                else:
                    exec('old_val[%s = new_val' % idx)
                self.scope.set(src, old_val, force=True)
            else:
                if isinstance(new_val, ndarray):
                    self.scope.set(src, new_val.copy(), force=True)
                else:
                    self.scope.set(src, new_val, force=True)

        #print 'after FD', self.pa.name, self.J
        return self.J

    def get_outputs(self, x):
        """Return matrix of flattened values from output edges."""

        for src in self.outputs:

            # Speedhack: getting an indexed var in OpenMDAO is slow
            if '[' in src:
                basekey, _, index = src.partition('[')
                base = self.scope.get(basekey)
                exec("src_val = base[%s" % index)
            else:
                src_val = self.scope.get(src)

            src_val = flattened_value(src, src_val)
            i1, i2 = self.out_bounds[src]
            if len(src_val) > 1:
                x[i1:i2] = src_val.copy()
            else:
                x[i1:i2] = src_val[0]

    def set_value(self, srcs, val, i1, i2, index, undo_complex=False):
        """Set a value in the model"""

        # Support for Parameter Groups:
        if isinstance(srcs, basestring):
            srcs = [srcs]

        # For keeping track of arrays that share the same memory.
        array_base_val = None
        index_base_val = None

        for src in srcs:
            comp_name, _, var_name = src.partition('.')
            comp = self.scope.get(comp_name)

            if i2-i1 == 1:

                # Indexed array
                src, _, idx = src.partition('[')
                if idx:
                    old_val = self.scope.get(src)
                    if old_val is not array_base_val or \
                       idx != index_base_val:
                        exec('old_val[%s += val' % idx)
                        array_base_val = old_val
                        index_base_val = idx

                    # In-place array editing doesn't activate callback, so we
                    # must do it manually.
                    if var_name:
                        base = self.scope._depgraph.base_var(src)
                        comp._input_updated(base.split('.')[-1],
                                            src.split('[')[0].partition('.')[2])
                    else:
                        self.scope._input_updated(comp_name.split('[')[0])

                # Scalar
                else:
                    old_val = self.scope.get(src)
                    if undo_complex is True:
                        self.scope.set(src, (old_val+val).real, force=True)
                    else:
                        self.scope.set(src, old_val+val, force=True)

            # Full vector
            else:
                idx = index - i1

                # Indexed array
                if '[' in src:
                    base_src, _, base_idx = src.partition('[')
                    base_val = self.scope.get(base_src)
                    if base_val is not array_base_val or \
                       base_idx != index_base_val:
                        # Note: could speed this up with an eval
                        # (until Bret looks into the expression speed)
                        sliced_src = self.scope.get(src)
                        sliced_shape = sliced_src.shape
                        flattened_src = sliced_src.flatten()
                        flattened_src[idx] += val
                        sliced_src = flattened_src.reshape(sliced_shape)
                        exec('self.scope.%s = sliced_src') % src
                        array_base_val = base_val
                        index_base_val = base_idx

                else:

                    old_val = self.scope.get(src)
                    if old_val is not array_base_val:
                        unravelled = unravel_index(idx, old_val.shape)
                        old_val[unravelled] += val
                        array_base_val = old_val

                # In-place array editing doesn't activate callback, so we must
                # do it manually.
                if var_name:
                    base = self.scope._depgraph.base_var(src)
                    comp._input_updated(base.split('.')[-1],
                                        src.split('[')[0].partition('.')[2])
                else:
                    self.scope._input_updated(comp_name.split('[', 1)[0])

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

    def __init__(self, pa):
        """ Performs finite difference on the components in a given
        pseudo_assembly. """

        self.inputs = pa.inputs
        self.outputs = pa.outputs
        self.in_bounds = {}
        self.out_bounds = {}
        self.pa = pa
        self.scope = pa.wflow.scope

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
            self.out_bounds[src] = (out_size, out_size+width)
            out_size += width

        self.y_base = zeros((out_size,))
        self.y = zeros((out_size,))
        self.y2 = zeros((out_size,))

    def calculate(self, arg, result):
        """Return Jacobian of all outputs with respect to a given direction in
        the input space."""

        self.get_outputs(self.y_base)

        options = self.pa.wflow._parent.gradient_options
        fd_step = options.fd_step
        form = options.fd_form

        #--------------------
        # Forward difference
        #--------------------
        if form == 'forward':

            # Step
            self.set_value(fd_step, arg)

            self.pa.run(ffd_order=1)
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

            self.pa.run(ffd_order=1)
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

            self.pa.run(ffd_order=1)
            self.get_outputs(self.y)

            # Backward Step
            self.set_value(-2.0*fd_step, arg)

            self.pa.run(ffd_order=1)
            self.get_outputs(self.y2)

            # Central difference
            mv_prod = (self.y - self.y2)/(2.0*fd_step)

            # Undo step
            self.set_value(fd_step, arg)

        #--------------------
        # Complex Step
        #--------------------
        elif form == 'complex_step':

            complex_step = fd_step*1j
            self.pa.set_complex_step()
            yc = zeros(len(self.y), dtype=complex128)

            # Step
            self.set_value(complex_step, arg)

            self.pa.run(ffd_order=1)
            self.get_outputs(yc)

            # Forward difference
            mv_prod = (yc/fd_step).imag

            # Undo step
            self.set_value(-fd_step, arg, undo_complex=True)

        # Return outputs to a clean state.
        for j, src in enumerate(self.outputs):
            i1, i2 = self.out_bounds[src]
            old_val = self.scope.get(src)

            # Put answer into the right spot in result.
            key = self.pa.mapped_outputs[j]

            # This happens if an array is connected in full and in slice.
            # Only need to handle it in full.
            if key not in result:
                continue

            if isinstance(old_val, (float, complex)):
                new_val = float(self.y_base[i1:i2])
                result[key] += mv_prod[i1:i2]
            elif isinstance(old_val, ndarray):
                shape = old_val.shape
                if len(shape) > 1:
                    new_val = self.y_base[i1:i2]
                    new_val = new_val.reshape(shape)
                    result[key] += mv_prod[i1:i2].reshape(shape)
                else:
                    result[key] += mv_prod[i1:i2]
                    new_val = self.y_base[i1:i2]
            elif has_interface(old_val, IVariableTree):
                new_val = old_val.copy()
                self.pa.wflow._update(src, new_val, self.y_base[i1:i2])
                result[key] += mv_prod[i1:i2]
            else:
                continue

            src, _, idx = src.partition('[')
            if idx:
                old_val = self.scope.get(src)
                if isinstance(new_val, ndarray):
                    exec('old_val[%s = new_val.copy()' % idx)
                else:
                    exec('old_val[%s = new_val' % idx)
                self.scope.set(src, old_val, force=True)
            else:
                if isinstance(new_val, ndarray):
                    self.scope.set(src, new_val.copy(), force=True)
                else:
                    self.scope.set(src, new_val, force=True)

        #print mv_prod, arg, result

    def set_value(self, fdstep, arg, undo_complex=False):
        """Set a value in the model"""

        for j, src_tuple in enumerate(self.inputs):

            # Support for Parameter Groups:
            if isinstance(src_tuple, basestring):
                src_tuple = (src_tuple,)

            i1, i2 = self.in_bounds[src_tuple[0]]

            # For keeping track of arrays that share the same memory.
            array_base_val = None
            index_base_val = None

            key = self.pa.mapped_inputs[j]
            if not isinstance(key, basestring):
                key = key[0]

            direction = arg[key]*fdstep

            if i2-i1 == 1:
                direction = direction[0]

            for src in src_tuple:

                comp_name, _, var_name = src.partition('.')
                comp = self.scope.get(comp_name)

                # Indexed array
                src, _, idx = src.partition('[')
                if idx:
                    old_val = self.scope.get(src)
                    if old_val is not array_base_val or \
                       idx != index_base_val:
                        exec('old_val[%s += direction' % idx)
                        array_base_val = old_val
                        index_base_val = idx

                    # In-place array editing doesn't activate callback, so we
                    # must do it manually.
                    if var_name:
                        base = self.scope._depgraph.base_var(src)
                        comp._input_updated(base.split('.')[-1],
                                            src.split('[')[0].partition('.')[2])
                    else:
                        self.scope._input_updated(comp_name.split('[')[0])

                # Whole Variable
                else:
                    old_val = self.scope.get(src)
                    if undo_complex is True:
                        self.scope.set(src, (old_val+direction).real, force=True)
                    else:
                        self.scope.set(src, old_val+direction, force=True)

    def get_outputs(self, x):
        """Return matrix of flattened values from output edges."""

        for src in self.outputs:

            # Speedhack: getting an indexed var in OpenMDAO is slow
            if '[' in src:
                basekey, _, index = src.partition('[')
                base = self.scope.get(basekey)
                exec("src_val = base[%s" % index)
            else:
                src_val = self.scope.get(src)

            src_val = flattened_value(src, src_val)
            i1, i2 = self.out_bounds[src]
            if len(src_val) > 1:
                x[i1:i2] = src_val.copy()
            else:
                x[i1:i2] = src_val[0]
