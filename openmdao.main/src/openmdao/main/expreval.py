# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]

import weakref
import math
import ast
import __builtin__

from openmdao.main.printexpr import _get_attr_node, _get_long_name, \
                                    transform_expression, ExprPrinter, \
                                    print_node
from openmdao.main.index import INDEX, ATTR, CALL, SLICE, EXTSLICE

def _import_functs(mod, dct, names=None):
    if names is None:
        names = dir(mod)
    for name in names:
        if not name.startswith('_'):
            dct[name] = getattr(mod, name)

# this dict will act as the local scope when we eval our expressions
_expr_dict = {
    'math': math,
    }
# add stuff from math lib directly to our locals dict so users won't have to
# put 'math.' in front of all of their calls to standard math functions


# make numpy functions available if possible
import numpy
names = ['array', 'cosh', 'ldexp', 'hypot', 'tan', 'isnan', 'log', 'fabs',
'floor', 'sqrt', 'frexp', 'degrees', 'pi', 'log10', 'modf',
'copysign', 'cos', 'ceil', 'isinf', 'sinh', 'trunc',
'expm1', 'e', 'tanh', 'radians', 'sin', 'fmod', 'exp', 'log1p']
_import_functs(numpy, _expr_dict, names=names)

_expr_dict['pow'] = numpy.power #pow in math is not complex stepable, but this one is!

math_names = ['asin', 'asinh', 'atanh', 'atan', 'atan2', 'factorial',
'fsum', 'lgamma', 'erf', 'erfc', 'acosh', 'acos', 'gamma']
_import_functs(math, _expr_dict, names=math_names)

_expr_dict['numpy'] = numpy

# if scipy is available, add some functions
try:
    import scipy.special
except ImportError:
    pass
else:
    _import_functs(scipy.special, _expr_dict, names=['gamma', 'polygamma'])


from numpy import ndarray, ndindex, zeros, complex, imag, issubdtype


_Missing = object()

def find_dotted(scope, vname):
    """Return the object corresponding to the given name in the given
    scope. Returns _Missing if object is not found.
    """
    vname = vname.split('[', 1)[0]
    obj = scope
    for name in vname.split('.'):
        obj = getattr(obj, name, _Missing)
        if obj is _Missing:
            return obj
    return obj

def in_expr_locals(scope, name):
    """Return True if the given (dotted) name refers to something in our
    _expr_dict dict, e.g., math.sin.  Raises a KeyError if the name
    refers to something in _expr_dict that doesn't exist, e.g., math.foobar.
    Returns False if the name refers to nothing in _expr_dict,
    e.g., mycomp.x.
    """
    if hasattr(scope, name):
        return False
    if hasattr(__builtin__, name) or name == '_local_setter_':
        return True
    parts = name.split('.')
    obj = _expr_dict.get(parts[0], _Missing)
    if obj is _Missing:
        return False
    for part in parts[1:]:
        obj = getattr(obj, part, _Missing)
        if obj is _Missing:
            raise KeyError("Can't find '%s' in current scope" % name)
    return True

class ExprVarScanner(ast.NodeVisitor):
    """This node visitor collects all attribute names (including dotted ones)
    that occur in the given AST.
    """
    def __init__(self):
        self.varnames = set()

    def visit_Name(self, node):
        self.varnames.add(node.id)

    def visit_Attribute(self, node):
        long_name = _get_long_name(node)
        if long_name:
            self.varnames.add(long_name)

    def get_var_names(self, scope):
        """Returns a tuple of the form (local_vars, external_vars)."""
        local_vars = []
        extern_vars = []

        for v in self.varnames:
            if in_expr_locals(scope, v):
                continue
            if find_dotted(scope, v) is not _Missing:
                local_vars.append(v)
            else:
                extern_vars.append(v)

        return (local_vars, extern_vars)


class ExprTransformer(ast.NodeTransformer):
    """Transforms dotted name references, e.g., abc.d.g in an expression AST
    into scope.get('abc.d.g') and turns assignments into the appropriate
    set() calls. Also, translates function calls and indirect attribute
    accesses into a form that can be passed to a downstream object and
    executed there. For example, abc.d[xyz](1, pdq-10).value would translate
    to, e.g., scope.get('abc.d', [(0,xyz), (0,[1,pdq-10]), (1,'value')]).
    """
    def __init__(self, expreval, rhs=None, getter='get'):
        self.expreval = expreval
        self.rhs = rhs
        self._stack = []  # use this to see if we're inside of parens or
                          # brackets so that we always translate to 'get'
                          # even if we're on the lhs
        self.getter = getter
        super(ExprTransformer, self).__init__()

    def visit(self, node, subs=None):
        """Visit a node."""
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        if visitor == self.generic_visit:
            return visitor(node)
        else:
            return visitor(node, subs)

    def _name_to_node(self, node, name, subs=None):
        """Given a dotted name, return the proper node depending on whether
        the name is resolvable in 'local' scope or not.
        """
        if name is None:
            return super(ExprTransformer, self).generic_visit(node)

        if in_expr_locals(self.expreval.scope, name):
            return node

        names = ['scope']
        self.expreval.var_names.add(name)

        args = [ast.Str(s=name)]
        if self.rhs and len(self._stack) == 0:
            fname = 'set'
            args.append(self.rhs)
        else:
            fname = self.getter
        keywords = []
        names.append(fname)

        called_obj = _get_attr_node(names)
        if subs:
            args.append(ast.List(elts=subs, ctx=ast.Load()))

        return ast.copy_location(ast.Call(func=called_obj, args=args,
                                          ctx=node.ctx, keywords=keywords), node)

    def visit_Name(self, node, subs=None):
        return self._name_to_node(node, node.id, subs)

    def visit_Attribute(self, node, subs=None):
        long_name = _get_long_name(node)
        if long_name is None:
            # this Attribute contains more than just names/attrs
            if subs is None:
                subs = []
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=ATTR), ast.Str(s=node.attr)],
                                   ctx=ast.Load())]
            newnode = self.visit(node.value, subs)
            if newnode is node.value:
                return node
            return newnode
        return self._name_to_node(node, long_name, subs)

    def _get_slice_vals(self, node):
        lower = ast.Name(id='None', ctx=ast.Load()) if node.lower is None else self.visit(node.lower)
        upper = ast.Name(id='None', ctx=ast.Load()) if node.upper is None else self.visit(node.upper)
        step = ast.Name(id='None', ctx=ast.Load()) if node.step is None else self.visit(node.step)
        return ast.Tuple(elts=[lower, upper, step], ctx=ast.Load())

    def visit_Subscript(self, node, subs=None):
        self._stack.append(node)
        if subs is None:
            subs = []
        if isinstance(node.slice, ast.Index):
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=INDEX), self.visit(node.slice.value)],
                                   ctx=ast.Load())]
        elif isinstance(node.slice, ast.Slice):
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=SLICE), self._get_slice_vals(node.slice)],
                                   ctx=ast.Load())]
        elif isinstance(node.slice, ast.ExtSlice):
            elts = [ast.Num(n=EXTSLICE)]
            for val in node.slice.dims:
                if isinstance(val, ast.Slice):
                    elts.append(self._get_slice_vals(val))
                else:
                    elts.append(self.visit(val.value))
            subs[0:0] = [ast.Tuple(elts=elts, ctx=ast.Load())]
        else:
            raise ValueError("unknown Subscript child node: %s"
                             % node.slice.__class__.__name__)
        self._stack.pop()

        newnode = self.visit(node.value, subs)
        if newnode is node.value:
            return node
        elif isinstance(newnode, ast.Attribute):
            node.value = newnode
            node.slice = self.generic_visit(node.slice)
            return node
        return newnode

    def visit_Call(self, node, subs=None):
        name = _get_long_name(node.func)
        if name is not None:
            if in_expr_locals(self.expreval.scope, name) or '.' not in name:
                return self.generic_visit(node)

        if subs is None:
            subs = []

        self._stack.append(node)

        call_list = []

        if hasattr(node, 'kwargs') and node.kwargs:
            if isinstance(node.kwargs, ast.Name):
                raise SyntaxError("Can't translate '**%s'" % node.kwargs.id)
            else:
                raise SyntaxError("Can't translate '**' arguments")

        if hasattr(node, 'starargs') and node.starargs:
            if isinstance(node.starargs, ast.Name):
                raise SyntaxError("Can't translate '**%s'" % node.starargs.id)
            else:
                raise SyntaxError("Can't translate '*' arguments")

        if hasattr(node, 'keywords'):
            elts = [ast.Tuple(elts=[ast.Str(kw.arg), self.visit(kw.value)],
                              ctx=ast.Load()) for kw in node.keywords]
            if len(call_list) > 0 or len(elts) > 0:
                call_list.append(ast.List(elts=elts, ctx=ast.Load()))

        if len(node.args) > 0 or len(call_list) > 0:
            call_list.append(ast.List(elts=[self.visit(arg) for arg in node.args],
                                      ctx=ast.Load()))

        self._stack.pop()

        # call_list is reversed here because we built it backwards in order
        # to make it a little easier to leave off unnecessary empty stuff
        subs[0:0] = [ast.Tuple(elts=[ast.Num(n=CALL)]+call_list[::-1],
                               ctx=ast.Load())]

        return self.visit(node.func, subs)

    def visit_Module(self, node, subs=None):
        # Make sure there is only one statement or expression
        if len(node.body) > 1 or \
           (node.body and not isinstance(node.body[0], (ast.Assign, ast.Expr))):
            raise RuntimeError("Only one assignment statement or expression"
                               " is allowed")
        top = super(ExprTransformer, self).generic_visit(node)
        if top.body and isinstance(top.body[0], ast.Call):
            top.body[0] = ast.Expr(value=top.body[0])
        return top

    def visit_Assign(self, node, subs=None):
        if len(node.targets) > 1:
            raise RuntimeError("only one expression is allowed on left hand"
                               " side of assignment")
        rhs = self.visit(node.value)
        lhs = ExprTransformer(self.expreval, rhs=rhs).visit(node.targets[0])
        if isinstance(lhs, (ast.Name, ast.Subscript, ast.Attribute)):
            lhs.ctx = ast.Store()
            return ast.Assign(targets=[lhs], value=rhs)
        return lhs

class ExprExaminer(ast.NodeVisitor):
    """"Examines various properties of an expression for later analysis."""
    def __init__(self, node, evaluator=None):
        super(ExprExaminer, self).__init__()
        self._in_idx = False
        self.ref_ok = True
        self.const = True
        self.simplevar = True  # if true, it's just a simple variable name
                               # (possibly with dots)
        self.refs = set() # variables and/or subscripted variables referenced
                          # in this expression
        self.const_indices = True
        self.assignable = True
        self._evaluator = evaluator

        self.visit(node)

    def _maybe_add_ref(self, name):
        """Will add a ref if it's not a name from the locals dict."""
        if name != 'None' and self._in_idx:
            self.const_indices = False
        if not self.ref_ok:
            return
        if self._evaluator and in_expr_locals(self._evaluator.scope, name):
            return
        self.refs.add(name)

    def visit_Index(self, node):
        self.simplevar = self.const = False
        self.visit(node.value)

    def visit_Assign(self, node):
        self.assignable = False
        self.const = False
        self.simplevar = False
        self.generic_visit(node)

    def visit_Slice(self, node):
        self.simplevar = self.const = False
        self.generic_visit(node)

    def visit_ExtSlice(self, node):
        self.simplevar = self.const = False
        for d in node.dims:
            self.visit(d)

    def visit_Name(self, node):
        self.const = False
        self._maybe_add_ref(node.id)
        self.generic_visit(node)

    def visit_Attribute(self, node):
        self.const = False
        long_name = _get_long_name(node)
        if long_name:
            self._maybe_add_ref(long_name)
        else:
            self.simplevar = False
            self.generic_visit(node)

    def visit_Subscript(self, node):
        self.const = False
        p = ExprPrinter()
        p.visit(node)
        self._maybe_add_ref(p.get_text())
        ok = self.ref_ok
        self.ref_ok = False
        self.visit(node.value)
        old = self._in_idx
        self._in_idx = True
        self.visit(node.slice)
        self._in_idx = old
        self.ref_ok = ok

    def visit_Num(self, node):
        self.simplevar = False
        if self.const:
            self.assignable = False
        self.generic_visit(node)

    def _no_assign(self, node):
        self.assignable = self.simplevar = False
        self.generic_visit(node)

    visit_Load       = ast.NodeVisitor.generic_visit
    visit_Store      = ast.NodeVisitor.generic_visit
    visit_Expr       = ast.NodeVisitor.generic_visit
    visit_Expression = ast.NodeVisitor.generic_visit

    visit_Call       = _no_assign
    visit_USub       = _no_assign
    visit_UAdd       = _no_assign
    visit_And        = _no_assign
    visit_Or         = _no_assign

    # operators
    visit_Add        = _no_assign
    visit_Sub        = _no_assign
    visit_Mult       = _no_assign
    visit_Div        = _no_assign
    visit_Mod        = _no_assign
    visit_Pow        = _no_assign
    visit_LShift     = _no_assign
    visit_Rshift     = _no_assign
    visit_BitOr      = _no_assign
    visit_BitXor     = _no_assign
    visit_BitAnd     = _no_assign
    visit_FloorDiv   = _no_assign

    # cmp operators
    visit_Eq         = _no_assign
    visit_NotEq      = _no_assign
    visit_Lt         = _no_assign
    visit_LtE        = _no_assign
    visit_Gt         = _no_assign
    visit_GtE        = _no_assign
    visit_Is         = _no_assign
    visit_IsNot      = _no_assign
    visit_In         = _no_assign
    visit_NotIn      = _no_assign


class ExprEvaluator(object):
    """A class that translates an expression string into a new string
    containing any necessary framework access functions, e.g., set, get. The
    compiled bytecode is stored within the object so that it doesn't have to
    be reparsed during later evaluations. A scoping object is required at
    construction time or evaluation time, and that object determines the form
    of the translated expression. Array entry access, 'downstream' attribute
    access, and function invocation are also translated in a similar way.
    For a description of the format of the 'index' arg of set/get that is
    generated by ExprEvaluator, see the doc string for the
    ``openmdao.main.index.process_index_entry`` function.
    """

    def __init__(self, text, scope=None, getter='get'):
        self._scope = None
        self.scope = scope
        self.text = text
        self.getter = getter
        self.var_names = set()
        self.cached_grad_eq = None

    @property
    def text(self):
        """The expression string."""
        return self._text

    @text.setter
    def text(self, value):
        self._code = self._assignment_code = None
        self._examiner = self.cached_grad_eq = None
        self._text = value

    @property
    def scope(self):
        """The scoping object used to evaluate the expression."""
        if self._scope:
            scope = self._scope()
            if scope is None:
                raise RuntimeError('ExprEvaluator scoping object no longer'
                                   ' exists.')
            return scope
        return None

    @scope.setter
    def scope(self, value):
        scp = None if self._scope is None else self._scope()
        if scp is None or value is not scp:
            self._code = self._assignment_code = None
            self._examiner = self.cached_grad_eq = None
            if value is not None:
                self._scope = weakref.ref(value)
            else:
                self._scope = None

    @classmethod
    def _invalid_expression_error(cls, unresolved_vars, expr=None, msg=None):
        """
        Creates and returns an invalid expression error that can be raised.
        Also adds the unresolved variables as an attribute to the error.
        This is so the message can be more specifically tailored by catching
        the error, creating your own message, and passing the necessary
        arguments to generate a new error.

        An example of this can be seen in Constraint.__init__.

        unresolved_vars: list of unresolved variables
        expr: Expression string
        msg: Message with {0} and {1} placeholders to be formatted.
             {0} will be replaced by expr and {1} will be replaced
             by the unresolved variables

        """
        if not msg:
            msg = "Expression '{0}' has invalid variables {1}"

        if not expr:
            expr = cls.text

        #do some formatting for the error message
        #wrap the variables in single quotes
        formatted_vars = ["'{0}'".format(var) for var in unresolved_vars]

        #if there is more than one variable,
        #seperate the variables with commas
        if len(formatted_vars) == 1:
            formatted_vars = ''.join(formatted_vars)
        else:
            formatted_vars = ', '.join(formatted_vars)

        #throw the error
        return ValueError(msg.format(expr, formatted_vars))

    def is_valid_assignee(self):
        """Returns True if the syntax of our expression is valid to
        be on the left-hand side of an assignment.  No check is
        performed to see if the variable(s) in the expression actually
        exist.
        """
        if self._code is None:
            self._pre_parse()
        return self._allow_set

    def refers_to(self, name):
        """Returns True if this expression refers to the given variable or
        component.
        """
        if name == self.text:
            return True
        elif name in self.text:
            if name in self.get_referenced_varpaths(copy=False):
                return True
            if name in self.get_referenced_compnames():
                return True
        return False

    def __getstate__(self):
        """Return dict representing this container's state."""
        state = self.__dict__.copy()
        # remove weakref to scope because it won't pickle
        state['_scope'] = self.scope
        state['_code'] = None  # <type 'code'> won't pickle either.
        state['cached_grad_eq'] = None
        if state.get('_assignment_code'):
            state['_assignment_code'] = None # more unpicklable <type 'code'>
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        self.__dict__.update(state)
        if self._scope is not None:
            self._scope = weakref.ref(self._scope)

    def _pre_parse(self):
        try:
            root = ast.parse(self.text, mode='eval')
        except SyntaxError:
            # might be an assignment, try mode='exec'
            root = ast.parse(self.text, mode='exec')
            self._allow_set = False
            return root

        if not isinstance(root.body,
                          (ast.Attribute, ast.Name, ast.Subscript)):
            self._allow_set = False
        else:
            self._allow_set = True
        return root

    def _parse_get(self):
        astree = self._pre_parse()

        new_ast = ExprTransformer(self, getter=self.getter).visit(astree)

        # compile the transformed AST
        ast.fix_missing_locations(new_ast)
        mode = 'exec' if isinstance(new_ast, ast.Module) else 'eval'
        return (new_ast, compile(new_ast, '<string>', mode))

    def _parse_set(self):
        self._pre_parse()
        if not self._allow_set:
            raise ValueError("expression '%s' can't be set to a value"
                             % self.text)
        root = ast.parse("%s=_local_setter_" % self.text, mode='exec')
        ## transform into a 'set' call to set the specified variable
        assign_ast = ExprTransformer(self, getter=self.getter).visit(root)
        ast.fix_missing_locations(assign_ast)
        code = compile(assign_ast, '<string>', 'exec')
        return (assign_ast, code)

    def _parse(self):
        self.var_names = set()
        try:
            new_ast, self._code = self._parse_get()
        except SyntaxError as err:
            raise SyntaxError("failed to parse expression '%s': %s"
                              % (self.text, str(err)))
        return new_ast

    def _get_updated_scope(self, scope):
        if scope is not None:
            self.scope = scope
            return scope
        return self.scope

    def evaluate(self, scope=None):
        """Return the value of the scoped string, evaluated
        using the eval() function.
        """
        if scope is None:
            scope = self.scope
        else:
            self.scope = scope

        try:
            if self._code is None:
                self._parse()
            return eval(self._code, _expr_dict, locals())
        except Exception, err:
            raise type(err)("can't evaluate expression "
                            "'%s': %s" % (self.text, str(err)))

    def refs(self, copy=True):
        """Returns a list of all variables referenced,
        including any array indices."""
        if self._code is None:
            self._parse()
        if self._examiner is None:
            self._examiner = ExprExaminer(ast.parse(self.text,
                                                    mode='eval'), self)
        if copy:
            return self._examiner.refs.copy()
        else:
            return self._examiner.refs

    def _finite_difference(self, grad_code, var_dict, target_var, stepsize, index=None):
        """ Perform central difference
        """

        if index:
            var_dict[target_var][index] += 0.5*stepsize
        else:
            var_dict[target_var] += 0.5*stepsize

        yp = eval(grad_code, _expr_dict, locals())

        if(isinstance(yp, ndarray)):
            yp = yp.flatten()

        if index:
            var_dict[target_var][index] -= stepsize
        else:
            var_dict[target_var] -= stepsize

        ym = eval(grad_code, _expr_dict, locals())

        if isinstance(ym, ndarray):
            ym = ym.flatten()

        grad = (yp - ym) / stepsize

        return grad

    def _complex_step(self, grad_code, var_dict, target_var, stepsize, index=None):
        """ Perform complex step
        """

        if index:
            var_dict[target_var][index] += stepsize * 1j
        else:
            var_dict[target_var] += stepsize * 1j

        yp = eval(grad_code, _expr_dict, locals())

        if(isinstance(yp, ndarray)):
            yp = yp.flatten()
            if not issubdtype(yp.dtype, complex):
                return None

            return imag(yp/stepsize)

        elif not isinstance(yp, complex):
            return None
        else:
            # note, imag returns a 0-d array, Don't know why.
            return imag(yp/stepsize).reshape(1, )[0]

        return imag(yp/stepsize)

    def evaluate_gradient(self, stepsize=1.0e-6, wrt=None, scope=None):
        """Return a dict containing the gradient of the expression with respect
        to each of the referenced varpaths. The gradient is calculated by 1st
        order central difference for now.

        stepsize: float
            Step size for finite difference.

        wrt: list of varpaths
            Varpaths for which we want to calculate the gradient.
        """
        scope = self._get_updated_scope(scope)
        inputs = list(self.refs(copy=False))

        if wrt is None:
            wrt = inputs
        elif isinstance(wrt, str):
            wrt = [wrt]

        var_dict = {}
        new_names = {}
        for name in inputs:
            if '[' in name:
                new_expr = ExprEvaluator(name, scope)
                replace_val = new_expr.evaluate()
            else:
                replace_val = scope.get(name)

            if isinstance(replace_val, ndarray):
                replace_val = replace_val.astype(numpy.complex)
            else:
                replace_val = float(replace_val)

            var_dict[name] = replace_val
            new_name = "var_dict['%s']" % name
            new_names[name] = new_name

        # First time through, cache our gradient code.
        if self.cached_grad_eq is None:

            grad_text = transform_expression(self.text, new_names)

            grad_root = ast.parse(grad_text, mode='eval')
            self.cached_grad_eq = compile(grad_root, '<string>', 'eval')

        grad_code = self.cached_grad_eq

        gradient = {}
        for var in wrt:

            # A "fake" boundary connection in an assembly has a special
            # format. All expression derivatives from inside the assembly are
            # handled outside the assembly.
            if var[0:4] == '@bin':
                gradient[var] = 1.0
                continue

            # Don't take derivative with respect to a variable that is not in
            # the expression
            if var not in inputs:
                gradient[var] = 0.0
                continue

            val = var_dict[var]
            if isinstance(val, ndarray):
                yp = eval(grad_code, _expr_dict, locals())

                if isinstance(yp, ndarray):
                    gradient[var] = zeros((yp.size, val.size))
                else:
                    gradient[var] = zeros((1, val.size))

                for i, index in enumerate(ndindex(*val.shape)):
                    try:
                        base = var_dict[var][index]
                        grad = self._complex_step(grad_code, var_dict,
                                                  var, stepsize, index)
                    except:
                        grad = None

                    if grad is None:
                        var_dict[var][index] = base
                        grad = self._finite_difference(grad_code, var_dict, var,
                                                       stepsize, index)
                    gradient[var][:, i] = grad
                    var_dict[var][index] = base

            else:
                try:
                    base = var_dict[var]
                    grad = self._complex_step(grad_code, var_dict, var, stepsize)
                except:
                    grad = None

                if grad is None:
                    var_dict[var] = base
                    grad = self._finite_difference(grad_code, var_dict, var,
                                                   stepsize)
                gradient[var] = grad
                var_dict[var] = base
                if isinstance(gradient[var], ndarray):
                    gradient[var] = gradient[var].reshape((gradient[var].size, 1))

        return gradient

    def set(self, val, scope=None, force=False):
        """Set the value of the referenced object to the specified value."""
        scope = self._get_updated_scope(scope)

        # self.assignment_code is a compiled version of an assignment
        # statement of the form 'somevar = _local_setter_', so we set
        # _local_setter_ here and the exec call will pull it out of the
        # locals dict.
        _local_setter_ = val
        _local_force_ = force
        if self._assignment_code is None:
            _, self._assignment_code = self._parse_set()
        exec(self._assignment_code, _expr_dict, locals())

    def get_metadata(self, metaname=None, scope=None):
        """Return the specified piece of metadata if metaname is provided.
        Otherwise return the whole metadata dictionary. If metaname is supplied
        but does not exist for a given variable, None will be returned for the
        variable.

        Returns a list of tuples containing (varname, metadata)
        corresponding to each variable referenced by this expression.
        """
        scope = self._get_updated_scope(scope)

        invalid_variables = []
        metadata = []

        for name in self.get_referenced_varpaths(copy=False):
            try:
                metadata.append((name, scope.get_metadata(name, metaname)))
            except AttributeError:
                invalid_variables.append(name)

        if invalid_variables:
            msg = "Couldn't find metadata for traits {traits}"
            traits = ', '.join("'{0}'".format(var) for var in invalid_variables)
            msg = msg.format(traits=traits)

            raise AttributeError(msg)

        return metadata

    def get_referenced_varpaths(self, copy=True, refs=False):
        """Return a set of pathnames relative to *scope.parent* and 
        based on the names of Variables referenced in our expression 
        string.  If refs is True, return full references that may 
        include not only the var name but also an array index, e.g.,
        'x[3]' instead of just 'x'.
        """
        if self._code is None:
            self._parse()
        if refs:
            return self.refs(copy)
        else:
            if copy:
                return self.var_names.copy()
            else:
                return self.var_names

    def get_referenced_compnames(self):
        """Return a set of Component names based on the pathnames of
        Variables referenced in our expression string. No checking is
        performed to verify that a given name refers to an actual Component.
        """
        if self._code is None:
            self._parse()
        nameset = set()
        for name in self.var_names:
            parts = name.split('.', 1)
            if len(parts) > 1:
                nameset.add(parts[0])
        return nameset

    def check_resolve(self):
        """Return True if all variables referenced by our expression can
        be resolved.
        """
        return len(self.get_unresolved()) == 0

    def get_unresolved(self):
        """Return a list of all variables that cannot be resolved."""
        if self._code is None:
            self._parse()
        if len(self.var_names) > 0:
            scope = self.scope
            if scope:
                return [n for n in self.var_names if not scope.contains(n)]
            return self.var_names.copy()
        return []

    def scope_transform(self, scope, new_scope, parent=None):
        """Return a transformed version of our text string where the attribute
        names are changed based on a change in scope to the given object.
        """
        if self._code is None:
            self._parse()

        oldname = scope.name + '.' if scope.name else ''
        newname = new_scope.name + '.' if new_scope.name else ''
        if scope is new_scope.parent or scope is parent:
            oldname = 'parent.'
        elif new_scope is scope.parent or new_scope is parent:
            newname = 'parent.'

        mapping = {}
        for var in self.get_referenced_varpaths(copy=False):
            if var.startswith(newname):
                mapping[var] = var[len(newname):]
            else:
                mapping[var] = oldname+var

        try:
            return transform_expression(self.text, mapping)
        except SyntaxError as err:
            raise SyntaxError("failed to transform expression '%s': %s"
                              % (self.text, str(err)))

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.text == other.text
        return False

    def __repr__(self):
        return '<ExprEval(text=%s)>' % self._text

    def __str__(self):
        return self._text

class ConnectedExprEvaluator(ExprEvaluator):
    """An ExprEvaluator that restricts the allowable syntax to only those
    expressions that can be connected within a model.  For example, array
    indexing is allowed, but all indices must be constants if the expression
    is on the destination side of a connection.
    """
    def __init__(self, *args, **kwargs):
        self._is_dest = kwargs.get('is_dest', False)
        if 'is_dest' in kwargs:
            del kwargs['is_dest']
        super(ConnectedExprEvaluator, self).__init__(*args, **kwargs)

    def _parse(self):
        super(ConnectedExprEvaluator, self)._parse()
        self._examiner = ExprExaminer(ast.parse(self.text, mode='eval'), self)
        if self._is_dest:
            if not self._examiner.const_indices:
                raise RuntimeError("bad destination expression '%s': only"
                                   " constant indices are allowed for arrays"
                                   " and slices" % self.text)
            if len(self._examiner.refs) != 1:
                raise RuntimeError("bad connected expression '%s' must"
                                   " reference exactly one variable" %
                                   self.text)
            if not self._examiner.assignable:
                raise RuntimeError("bad destination expression '%s': not"
                                   " assignable" % self.text)

    def refers_to(self, name):
        """Returns True if this expression refers to the given variable or
        component.
        """
        if super(ConnectedExprEvaluator, self).refers_to(name):
            return True
        return name in self.refs(copy=False)

if __name__ == '__main__':
    import sys
    from openmdao.main.container import build_container_hierarchy

    txt = ''.join(sys.argv[1:])
    root = ast.parse(txt, mode='exec')
    print 'original:\n %s' % txt

    print '\noriginal AST dump:'
    print ast.dump(root, annotate_fields=True)

    print '\nprinted AST:'
    ep = ExprPrinter()
    ep.visit(root)
    print ep.get_text()

    top = build_container_hierarchy({
        'a': {
            'b': { 'c': 1, 'd': 2 }
            },
        'x': {
            'y': { 'z': 3.14 }
            }
        })

    expreval = ExprEvaluator(txt, scope=top)
    root = ExprTransformer(expreval).visit(root)
    print '\ntransformed AST dump:'
    print ast.dump(root, annotate_fields=True)

    print '\nprinted transformed AST:'
    ep = ExprPrinter()
    ep.visit(root)
    print ep.get_text()

    print '\nvars referenced: %s' % expreval.get_referenced_varpaths(copy=False)

    print '\nattempting to compile the transformed AST...'
    ast.fix_missing_locations(root)
    code = compile(root, '<string>', 'exec')


