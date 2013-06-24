""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

from openmdao.main.api import VariableTree
from openmdao.main.derivatives import flattened_size, flattened_value
from openmdao.main.workflow import Workflow

try:
    from numpy import ndarray, zeros
except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, zeros

__all__ = ['SequentialWorkflow']


class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""

    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        self._names = []
        super(SequentialWorkflow, self).__init__(parent, scope, members)
        
        # Bookkeeping for calculating the residual.
        self._severed_edges = None
        self.res = None
        self.bounds = None        

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter(self.get_components())

    def __len__(self):
        return len(self._names)

    def __contains__(self, comp):
        return comp in self._names

    def index(self, comp):
        return self._names.index(comp)

    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_names(self):
        """Return a list of component names in this workflow."""
        return self._names[:]

    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the end of the workflow by name. """
        if isinstance(compnames, basestring):
            nodes = [compnames]
        else:
            nodes = compnames

        try:
            nodeit = iter(nodes)
        except TypeError:
            raise TypeError("Components must be added by name to a workflow.")

        for node in nodes:
            if isinstance(node, basestring):

                if check:
                    # check whether each node is valid and if not then
                    # construct a useful error message.
                    name = self._parent.parent.name
                    if not name:
                        name = "the top assembly."

                    # Components in subassys are never allowed.
                    if '.' in node:
                        msg = "Component '%s' is not" % node + \
                              " in the scope of %s" % name
                        raise AttributeError(msg)

                    # Does the component really exist?
                    try:
                        target = self._parent.parent.get(node)
                    except AttributeError:
                        msg = "Component '%s'" % node + \
                              " does not exist in %s" % name
                        raise AttributeError(msg)

                    # Don't add yourself to your own workflow
                    if target == self._parent:
                        msg = "You cannot add a driver to its own workflow"
                        raise AttributeError(msg)

                    # Check for circular dependency in driver workflow
                    if hasattr(target, 'iteration_set'):
                        iterset = target.iteration_set()
                        if self._parent in iterset:
                            msg = "Driver recursion loop detected"
                            raise AttributeError(msg)

                if index is None:
                    self._names.append(node)
                else:
                    self._names.insert(index, node)
                    index += 1
            else:
                raise TypeError("Components must be added by name to a workflow.")

        # We seem to need this so that our get_attributes is correct for the GUI.
        if check:
            self.config_changed()

    def remove(self, compname):
        """Remove a component from the workflow by name. Do not report an
        error if the specified component is not found.
        """
        if not isinstance(compname, basestring):
            raise TypeError("Components must be removed by name from a workflow.")
        try:
            self._names.remove(compname)
        except ValueError:
            pass

    def clear(self):
        """Remove all components from this workflow."""
        self._names = []

    def initialize_residual(self):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """
        nEdge = 0
        self.bounds = {}
        for edge in self.get_interior_edges():
            src = edge[0]
            val = self.scope.get(src)
            width = flattened_size(src, val)
            self.bounds[edge] = (nEdge, nEdge+width)
            nEdge += width

        # Initialize the residual vector on the first time through, and also
        # if for some reason the number of edges has changed.
        if self.res is None or nEdge != self.res.shape[0]:
            self.res = zeros((nEdge, 1))

        return nEdge

    def calculate_residuals(self):
        """Calculate and return the vector of residuals based on the current
        state of the system in our workflow."""
        for edge in self.get_interior_edges():
            src, target = edge
            src_val = self.scope.get(src)
            src_val = flattened_value(src, src_val).reshape(-1, 1)
            target_val = self.scope.get(target)
            target_val = flattened_value(target, target_val).reshape(-1, 1)
            i1, i2 = self.bounds[edge]
            self.res[i1:i2] = src_val - target_val

        return self.res

    def set_new_state(self, dv):
        """Adds a vector of new values to the current model state at the
        input edges.

        dv: ndarray (nEdge, 1)
            Array of values to add to the model inputs.
        """
        for edge in self._severed_edges:
            src, target = edge
            i1, i2 = self.bounds[edge]
            old_val = self.scope.get(target)

            if isinstance(old_val, float):
                new_val = old_val + float(dv[i1:i2])
            elif isinstance(old_val, ndarray):
                shape = old_val.shape
                if len(shape) > 1:
                    new_val = old_val.flatten() + dv[i1:i2]
                    new_val = new_val.reshape(shape)
                else:
                    new_val = old_val + dv[i1:i2]
            elif isinstance(old_val, VariableTree):
                new_val = old_val.copy()
                self._update(target, new_val, dv[i1:i2])
            else:
                msg = "Variable %s is of type %s." % (target, type(old_val)) + \
                      " This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)

            # Poke new value into the input end of the edge.
            self.scope.set(target, new_val, force=True)

            # Prevent OpenMDAO from stomping on our poked input.
            comp_name, dot, var_name = target.partition('.')
            comp = self.scope.get(comp_name)
            comp._valid_dict[var_name] = True

            #(An alternative way to prevent the stomping. This is more
            #concise, but setting an output and allowing OpenMDAO to pull it
            #felt hackish.)
            #self.scope.set(src, new_val, force=True)

    def _update(self, name, vtree, dv, i1=0):
        """ Update VariableTree `name` value `vtree` from `dv`. """
        for key in sorted(vtree.list_vars()):  # Force repeatable order.
            value = getattr(vtree, key)
            if isinstance(value, float):
                setattr(vtree, key, value + float(dv[i1]))
                i1 += 1
            elif isinstance(value, ndarray):
                shape = value.shape
                size = value.size
                i2 = i1 + size
                if len(shape) > 1:
                    value = value.flatten() + dv[i1:i2]
                    value = value.reshape(shape)
                else:
                    value = value + dv[i1:i2]
                setattr(vtree, key, value)
                i1 += size
            elif isinstance(value, VariableTree):
                i1 = self._update('.'.join((name, key)), value, dv, i1)
            else:
                msg = "Variable %s is of type %s." % (name, type(value)) + \
                      " This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)

        return i1

    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''

        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}

        # Start with zero-valued dictionaries cotaining keys for all inputs
        for comp in self:
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}

        # Fill input dictionaries with values from input arg.
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]

            comp_name, dot, var_name = src.partition('.')
            outputs[comp_name][var_name] = arg[i1:i2]
            inputs[comp_name][var_name] = arg[i1:i2]

            comp_name, dot, var_name = target.partition('.')
            inputs[comp_name][var_name] = arg[i1:i2]

        # Call ApplyJ on each component
        for comp in self:
            name = comp.name
            comp.applyJ(inputs[name], outputs[name])

        # Poke results into the return vector
        result = zeros(len(arg))
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]

            comp_name, dot, var_name = src.partition('.')
            result[i1:i2] = outputs[comp_name][var_name]

        return result

    def calc_gradient(self):
        """Returns the gradient of the given outputs with respect to all 
        parameters. The returned output is in the form of a dictionary of
        dictionaries where the 
        """
        pass
        
