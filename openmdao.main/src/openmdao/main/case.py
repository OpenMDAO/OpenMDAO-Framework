import time
import sys
from uuid import uuid1, getnode
from array import array
from StringIO import StringIO
from inspect import getmro
import weakref

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.exceptions import traceback_str, exception_str
from openmdao.main.variable import is_legal_name, make_legal_path
from openmdao.main.array_helpers import flattened_value

__all__ = ["Case"]

class MissingValue(object):
    def __str__(self):
        return "MissingValue"
    def __repr__(self):
        return "'MissingValue'"

_Missing = MissingValue()


def _simpleflatten(name, obj):
    return [(name, obj)]


def _flatten_dict(name, dct):
    ret = []

    def _recurse_flatten(ret, name, keys, dct):
        for k,v in dct.items():
            new_keys = keys+[k]
            if isinstance(v, (dict)):
                _recurse_flatten(ret, name, new_keys, v)
            else:
                keystr = '.'.join(['%s' % j for j in new_keys])
                ret.append(("%s.%s" % (name, keystr), v))
    _recurse_flatten(ret, name, [], dct)
    return ret


def _flatten_lst(name, lst):
    ret = []

    def _recurse_flatten(ret, name, idx, lst):
        for i, entry in enumerate(lst):
            new_idx = idx+[i]
            if isinstance(entry, (tuple, list, array)):
                # ret is flattened list so far
                # name is the name of the current object that needs to be flattened
                # new_idx is a list of indices for the dimensions of the item, e.g. [3,0] is the first item in the third item
                _recurse_flatten(ret, name, new_idx, entry)
            else:
                idxstr = ''.join(["[%d]" % j for j in new_idx])
                # returns the current flattened list if items
                # idxstr is a string representing what element in the list this is referring to, e.g. '[3][0]'
                ret.append(("%s%s" % (name, idxstr), entry))

    _recurse_flatten(ret, name, [], lst)
    return ret

flatteners = {
       int: _simpleflatten,
       float: _simpleflatten,
       str: _simpleflatten,
       unicode: _simpleflatten,
       list: _flatten_lst,
       tuple: _flatten_lst,
       array: _flatten_lst,
       dict: _flatten_dict,
       MissingValue: _simpleflatten,
    }

def flatten_obj(name, obj):
    f = flatteners.get(type(obj))
    if f:
        return f(name, obj)
    for klass in getmro(type(obj))[1:]:
        if klass in flatteners:
            return flatteners[klass](name, obj)
    # if cannot flatten return obj string
    if obj is not None:
        return [(name, '{%s}' % str(obj))]
    else:
        return [(name, '')]


class Case(object):
    """Contains all information necessary to specify an input *case*, i.e.,
    a list of names for all inputs to the case and their values. The case
    names may contain indexing into containers, attribute access, and/or
    function calls, as long as the full expression is valid as the
    left-hand side of an assignment. Outputs to be collected may also be
    added to the Case, and they can be more general expressions, i.e., they do
    not have to refer to a single variable. After the Case is executed, it
    will contain an indicator of the exit status of the case, a string
    containing error messages associated with the running of the case (if
    any), and a unique case identifier.

    """

    _uuid_node = getnode()
    _uuid_seq = 0

    @staticmethod
    def next_uuid():
        """Generate a unique identifier."""
        Case._uuid_seq += 1
        return str(uuid1(node=Case._uuid_node, clock_seq=Case._uuid_seq))

    def __init__(self, inputs=None, outputs=None, exc=None,
                 case_uuid=None, parent_uuid=''):
        """If inputs are supplied to the constructor, it must be an
        iterator that returns (name,value) tuples, where name is allowed
        to contain array notation and/or function calls. Outputs must be
        an iterator that returns strings containing names or expressions.

        """
        self._exprs = None
        self._outputs = None
        self._inputs = {}
        self.exc = exc  # a sys.exc_info() tuple

        if case_uuid:
            self.uuid = str(case_uuid)
        else:
            self.uuid = Case.next_uuid()
        self.parent_uuid = str(parent_uuid)  # identifier of parent case, if any

        self.timestamp = time.time()

        if inputs:
            self.add_inputs(inputs)
        if outputs:
            self.add_outputs(outputs)

    @property
    def msg(self):
        """Exception message."""
        return '' if self.exc is None else exception_str(self.exc)

    @property
    def traceback(self):
        """Exception traceback."""
        return '' if self.exc is None else traceback_str(self.exc)

    def __str__(self):
        if self._outputs:
            outs = self._outputs.items()
            outs.sort()
        else:
            outs = []
        ins = self._inputs.items()
        ins.sort()

        stream = StringIO()
        write = stream.write
        write("Case:\n")
        write("   uuid: %s\n" % self.uuid)
        write("   timestamp: %15f\n" % self.timestamp)
        if self.parent_uuid:
            write("   parent_uuid: %s\n" % self.parent_uuid)
        if ins:
            write("   inputs:\n")
            for name, val in ins:
                write("      %s: %s\n" % (name, val))
        if outs:
            write("   outputs:\n")
            for name, val in outs:
                write("      %s: %s\n" % (name, val))
        if self.exc:
            stream.write("   exc: %s\n" % exception_str(self.exc))
            stream.write("        %s\n" % traceback_str(self.exc))

        return stream.getvalue()

    def __eq__(self, other):
        if self is other:
            return True
        try:
            if len(self) != len(other):
                return False
            for selftup, othertup in zip(self.items(flatten=True),
                                         other.items(flatten=True)):
                if selftup[0] != othertup[0] or selftup[1] != othertup[1]:
                    return False
        except:
            return False
        return True

    def __getitem__(self, name):
        val = self._inputs.get(name, _Missing)
        if val is not _Missing:
            return val
        if self._outputs:
            return self._outputs[name]
        raise KeyError("'%s' not found" % name)

    def __setitem__(self, name, value):
        if self._outputs and name in self._outputs:
            self._outputs[name] = value
        elif name in self._inputs:
            self._inputs[name] = value
        else:
            raise KeyError("'%s' not found" % name)

    def __contains__(self, name):
        return name in self._inputs or (self._outputs and name in self._outputs)

    def __len__(self):
        if self._outputs is None:
            return len(self._inputs)
        else:
            return len(self._inputs) + len(self._outputs)

    def get_input(self, name):
        return self._inputs[name]

    def get_output(self, name):
        if self._outputs:
            return self._outputs[name]
        raise KeyError("'%s' not found" % name)

    def get_inputs(self, flatten=False):
        if flatten:
            ret = []
            for k, v in self._inputs.items():
                ret.extend(flatten_obj(k, v))
            return ret
        else:
            return self._inputs.items()

    def get_outputs(self, flatten=False):
        if flatten:
            ret = []
            for k, v in self._outputs.items():
                ret.extend(flatten_obj(k, v))
            return ret
        else:
            return self._outputs.items()

    def items(self, iotype=None, flatten=False):
        """Return a list of (name,value) tuples for variables/expressions
        in this Case.

        iotype: str or None
            If 'in', only inputs are returned.
            If 'out', only outputs are returned.
            If None (the default), inputs and outputs are returned.

        flatten: bool
            If True, split multi-part Variables (like VariableTrees and Arrays)
            into their constituents.
        """
        if iotype is None:
            if self._outputs:
                return self.get_inputs(flatten) + self.get_outputs(flatten)
            else:
                return self.get_inputs(flatten)
        elif iotype == 'in':
            return self.get_inputs(flatten)
        elif iotype == 'out':
            if self._outputs:
                return self.get_outputs(flatten)
            else:
                return []
        else:
            raise NameError("invalid iotype arg (%s). Must be 'in', 'out',"
                            " or None" % str(iotype))

    def keys(self, iotype=None, flatten=False):
        """Return a list of name/expression strings for this Case.

        iotype: str or None
            If 'in', only inputs are returned.
            If 'out', only outputs are returned.
            If None (the default), inputs and outputs are returned.
        """
        return [k for k, v in self.items(iotype, flatten=flatten)]

    def values(self, iotype=None, flatten=False):
        """Return a list of values for this Case.

        iotype: str or None
            If 'in', only inputs are returned.
            If 'out', only outputs are returned
            If None (the default), inputs and outputs are returned
        """
        return [v for k, v in self.items(iotype, flatten=flatten)]

    def reset(self):
        """Remove any saved output values, get a new uuid and reset the
        parent_uuid.  Essentially this Case becomes like a new Case with the
        same set of inputs and outputs that hasn't been executed yet.
        """
        self.parent_uuid = ''
        self.uuid = str(uuid1())
        for key in self._outputs.keys():
            self._outputs[key] = _Missing

    def apply_inputs(self, scope):
        """Take the values of all of the inputs in this case and apply them
        to the specified scope.
        """
        scope._case_uuid = self.uuid
        for name, value in self._inputs.items():
            expr = self._exprs.get(name)
            if expr:
                expr.set(value, scope) #, tovector=True)
            else:
                scope.set(name, value)
                # FIXME: this extra setting of the vector is messy...
                if hasattr(scope, '_system'):
                    system = scope._system
                    if system is not None:
                        uvec = system.vec.get('u')
                        if uvec and name in uvec:
                            uvec[name][:] = flattened_value(name, value)

    def update_outputs(self, scope):
        """Update the value of all outputs in this Case, using the given scope.
        """
        last_excpt = None
        outputs = self._outputs
        if outputs is not None:
            exprs = self._exprs
            if exprs:
                for name in outputs.keys():
                    expr = exprs.get(name)
                    try:
                        if expr:
                            outputs[name] = expr.evaluate(scope)
                        else:
                            outputs[name] = scope.get(name)
                    except Exception:
                        last_excpt = sys.exc_info()
                        outputs[name] = _Missing
            else:
                for name in outputs.keys():
                    try:
                        outputs[name] = scope.get(name)
                    except Exception:
                        last_excpt = sys.exc_info()
                        outputs[name] = _Missing

        self.timestamp = time.time()

        if last_excpt is not None:
            raise last_excpt

    def add_input(self, name, value):
        """Adds an input and its value to this case.

        name: str
            Name of the input to be added. May contain an expression as long
            as it is valid when placed on the left-hand side of an assignment.

        value:
            Value that the input will be assigned to.
        """
        self._register_expr(name)
        self._inputs[name] = value

    def add_inputs(self, inp_iter):
        """Adds multiple inputs to this case.

        inp_iter: Iterator returning (name,value)
            Iterator of input names and values.
        """
        for name, value in inp_iter:
            self.add_input(name, value)

    def add_output(self, name, value=_Missing):
        """Adds an output to this case.

        name: str
            Name of output to be added.
        """
        self._register_expr(name)
        if self._outputs is None:
            self._outputs = {name: value}
        else:
            self._outputs[name] = value

    def add_outputs(self, outputs):
        """Adds outputs to this case.

        outputs: iterator returning names or tuples of the form (name,value)
            outputs to be added
        """
        for entry in outputs:
            if isinstance(entry, basestring):
                self.add_output(entry)
            else: # assume it's a tuple of the form (name, value)
                self.add_output(entry[0], entry[1])

    def subcase(self, names):
        """Return a new Case having a specified subset of this Case's inputs
        and outputs.
        """
        ins = []
        outs = []
        for name in names:
            val = self._inputs.get(name)
            if val is not None:
                ins.append((name, val))
            elif self._outputs:
                outs.append((name, self._outputs[name]))
            else:
                raise KeyError("'%s' is not part of this Case" % name)
        sc = Case(inputs=ins, outputs=outs, parent_uuid=self.parent_uuid)
        sc.timestamp = self.timestamp
        return sc

    def _register_expr(self, s):
        """If the given string contains an expression, create an ExprEvaluator
        and store it in self._exprs.
        """
        if not is_legal_name(s):
            expr = ExprEvaluator(s)
            if self._exprs is None:
                self._exprs = {}
            self._exprs[s] = expr

    @staticmethod
    def set_vartree_inputs(driver, cases):
        """ Set ``case_inputs`` vartree on `driver` from `cases`. """
        nans = [float('NaN')] * len(cases)
        parameters = driver.get_parameters()
        for path, value in cases[0].get_inputs():
            if path not in parameters:
                driver.add_parameter(path)
            path = make_legal_path(path)
            driver.set('case_inputs.'+path, nans)

        for i, case in enumerate(cases):
            for path, value in case.get_inputs():
                path = make_legal_path(path)
                driver.set('case_inputs.%s[%d]'%(path,i), value)


class CaseTreeNode(object):
    """ Represents a node in a tree of cases. Currently just for testing. """

    def __init__(self, case, parent):
        self.case = case
        self._parent = None
        self.parent = parent
        self.children = []

    @property
    def parent(self):
        return None if self._parent is None else self._parent()

    @parent.setter
    def parent(self, node):
        self._parent = None if node is None else weakref.ref(node)

    @staticmethod
    def sort(cases):
        """ Return forest populated from `cases`. """
        # Handles cases from tests, but not proven to handle an arbitrary
        # collection of cases.
        uuids = set()
        roots = []
        for case in cases:
            if case.uuid in uuids:
                raise RuntimeError('Duplicate uuid! %s' % case.uuid)
            uuids.add(case.uuid)

            node = CaseTreeNode(case, None)
            for root in list(roots):
                if case.uuid == root.case.parent_uuid:
                    root.parent = node
                    node.children.append(root)
                    roots.remove(root)
                else:
                    if root._insert(node):
                        break
            else:
                roots.append(node)

        # Rescan roots trying to consolidate the forest.
        old_len = len(roots)
        new_len = 0
        while new_len < old_len:
            old_len = len(roots)
            for node in list(roots):
                for root in list(roots):
                    if node is root:
                        continue
                    elif node.case.uuid == root.case.parent_uuid:
                        root.parent = node
                        node.children.append(root)
                        roots.remove(root)
                    elif root._insert(node):
                        roots.remove(node)
            new_len = len(roots)

        return roots

    def _insert(self, node):
        """ Return True if `node` has been inserted into tree. """
        if node.case.parent_uuid == self.case.uuid:
            self.children.append(node)
            return True
        else:
            for child in self.children:
                if child._insert(node):
                    return True
        return False

    def dump(self, level=0):
        """ Recursively display the tree. """
        prefix = '    '*level
        print '%suuid %s' % (prefix, self.case.uuid)
        print '%sparent uuid %s' % (prefix, self.case.parent_uuid)
        for name, value in self.case.get_outputs():
            if name.endswith('workflow.itername'):
                print '%sitername %s' % (prefix, value)
        print '%s#children %s' % (prefix, len(self.children))
        for child in self.children:
            child.dump(level+1)

    def iternames(self):
        """ Recursively scan for `workflow.itername`. """
        iternames = []
        for name, value in self.case.get_outputs():
            if name.endswith('workflow.itername'):
                iternames.append(value)
        for child in self.children:
            iternames.extend(child.iternames())
        return iternames

