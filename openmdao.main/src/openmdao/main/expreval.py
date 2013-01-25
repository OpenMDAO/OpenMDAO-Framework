# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]

import weakref
import math
import ast
import copy
import re
import __builtin__

from openmdao.main.printexpr import _get_attr_node, _get_long_name, transform_expression, ExprPrinter
from openmdao.util.nameutil import partition_names_by_comp
from openmdao.main.index import INDEX, ATTR, CALL, SLICE

from openmdao.main.sym import SymGrad, SymbolicDerivativeError

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
_import_functs(math, _expr_dict)
        
# make numpy functions available if possible
try:
    import numpy
except ImportError:
    pass
else:
    _expr_dict['numpy'] = numpy
    #_import_functs(numpy, _expr_dict, names=[])
    
# if scipy is available, add some functions
try:
    import scipy.special
except ImportError:
    pass
else:
    _import_functs(scipy.special, _expr_dict, names=['gamma', 'polygamma'])

_Missing = object()

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
        self._stack = []  # use this to see if we're inside of parens or brackets so
                          # that we always translate to 'get' even if we're on the lhs
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
        
        if self.expreval.is_local(name):
            return node
        
        #scope = self.expreval.scope
        #if scope:
        parts = name.split('.',1)
        names = ['scope']
        self.expreval.var_names.add(name)
        #else:
            #raise RuntimeError("expression has no scope")

        args = [ast.Str(s=name)]
        if self.rhs and len(self._stack) == 0:
            fname = 'set'
            args.append(self.rhs)
            keywords = [ast.keyword('src', ast.Name(id='_local_src_',
                                                    lineno=node.lineno,
                                                    col_offset=1,
                                                    ctx=ast.Load()))]
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
        if long_name is None: # this Attribute contains more than just names/attrs
            if subs is None:
                subs = []
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=ATTR),ast.Str(s=node.attr)], 
                                   ctx=ast.Load())]
            newnode = self.visit(node.value, subs)
            if newnode is node.value:
                return node
            return newnode
        return self._name_to_node(node, long_name, subs)

    def _get_slice_vals(self, node):
        lower = ast.Name(id='None',ctx=ast.Load()) if node.lower is None else self.visit(node.lower)
        upper = ast.Name(id='None',ctx=ast.Load()) if node.upper is None else self.visit(node.upper)
        step = ast.Name(id='None',ctx=ast.Load()) if node.step is None else self.visit(node.step)
        return ast.Tuple(elts=[lower, upper, step], ctx=ast.Load())
        
    def visit_Subscript(self, node, subs=None):
        self._stack.append(node)
        if subs is None:
            subs = []
        if isinstance(node.slice, ast.Index):
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=INDEX),self.visit(node.slice.value)], 
                                   ctx=ast.Load())]
        elif isinstance(node.slice, ast.Slice):
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=SLICE),self._get_slice_vals(node.slice)], 
                                   ctx=ast.Load())]
        else:
            raise ValueError("unknown Subscript child node: %s" % node.slice.__class__.__name__)

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
            if self.expreval.is_local(name) or '.' not in name:
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
            elts = [ast.Tuple(elts=[ast.Str(kw.arg),self.visit(kw.value)], ctx=ast.Load()) for kw in node.keywords]
            if len(call_list) > 0 or len(elts) > 0:
                call_list.append(ast.List(elts=elts, ctx=ast.Load()))

        if len(node.args)>0 or len(call_list)>0:
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
        if len(node.body) > 1 or (node.body and not isinstance(node.body[0], (ast.Assign, ast.Expr))):
            raise RuntimeError("Only one assignment statement or expression is allowed")
        top = super(ExprTransformer, self).generic_visit(node)
        if top.body and isinstance(top.body[0], ast.Call):
            top.body[0] = ast.Expr(value=top.body[0])
        return top
    
    def visit_Assign(self, node, subs=None):
        if len(node.targets) > 1:
            raise RuntimeError("only one expression is allowed on left hand side of assignment")
        rhs=self.visit(node.value)
        lhs = ExprTransformer(self.expreval, rhs=rhs).visit(node.targets[0])
        if isinstance(lhs, (ast.Name,ast.Subscript,ast.Attribute)):
            lhs.ctx = ast.Store()
            return ast.Assign(targets=[lhs], value=rhs)
        return lhs

class ExprExaminer(ast.NodeVisitor):
    """"Examines various properties of an expression for later analysis."""
    def __init__(self, node, evaluator=None):
        super(ExprExaminer, self).__init__()
        self.const = True
        self.simplevar = True  # if true, it's just a simple variable name (possibly with dots)
        self.refs= set()  # variables and/or subscripted variables referenced in this expression
        self.const_indices = True
        self.assignable = True
        self._evaluator = evaluator
        
        self.visit(node)
        
        # get rid of any refs that are just substrings of real refs, e.g., if the real ref is 'x[3]',
        # then there will also be a 'fake' ref for 'x'
        if len(self.refs) > 1:
            ep = ExprPrinter() # first we have to convert the ast back into a string
            ep.visit(node)
            txt = ep.get_text()
            # now we loop through the refs from longest to shortest, removing each from
            # the expression string.  As we get to each ref, we search for it in what's left
            # of the expression string. If we find it, then it's a real ref.
            for ref in sorted(self.refs, key=len, reverse=True):
                if ref not in txt:
                    self.refs.remove(ref)
                txt = txt.replace(ref, '')

    def _maybe_add_ref(self, name):
        """Will add a ref if it's not a name from the locals dict."""
        if self._evaluator and self._evaluator.is_local(name):
            return
        self.refs.add(name)

    def visit_Index(self, node):
        self.simplevar = self.const = False
        if not isinstance(node.value, ast.Num):
            if not (isinstance(node.value, ast.Tuple) and len(node.value.elts)==0):
                self.const_indices = False
        self.visit(node.value)

    def visit_Assign(self, node):
        self.assignable = False
        self.const = False
        self.simplevar = False
        super(ExprExaminer, self).generic_visit(node)
        
    def visit_Slice(self, node):
        self.simplevar = self.const = False
        if node.lower is not None:
            if not isinstance(node.lower, ast.Num):
                self.const_indices = False
            self.visit(node.lower)
        if node.upper is not None:
            if not isinstance(node.upper, ast.Num):
                self.const_indices = False
            self.visit(node.upper)
        if node.step is not None:
            if not isinstance(node.step, ast.Num):
                # for the step parameter, if it's None, that really means 1, which is constant,
                # unlike lower and upper which can vary depending upon the size of the containing
                # array at any given time
                if not(isinstance(node.step, ast.Name) and node.step.id == 'None'):
                    self.const_indices = False
            self.visit(node.step)

    def visit_Name(self, node):
        self.const = False
        self._maybe_add_ref(node.id)
        super(ExprExaminer, self).generic_visit(node)
        
    def visit_Attribute(self, node):
        self.const = False
        long_name = _get_long_name(node)
        if long_name:
            self._maybe_add_ref(long_name)
        else:
            self.simplevar = False
            super(ExprExaminer, self).generic_visit(node)
        
    def visit_Subscript(self, node):
        self.const = False
        p = ExprPrinter()
        p.visit(node)
        self._maybe_add_ref(p.get_text())
        super(ExprExaminer, self).generic_visit(node)
        
    def visit_Num(self, node):
        self.simplevar = False
        if self.const:
            self.assignable = False
        super(ExprExaminer, self).generic_visit(node)

    def _ignore(self, node):
        super(ExprExaminer, self).generic_visit(node)
        
    def _no_assign(self, node):
        self.assignable = self.simplevar = False
        super(ExprExaminer, self).generic_visit(node)
        
    visit_Load       = _ignore
    visit_Store      = _ignore
    visit_Expr       = _ignore
    visit_Expression = _ignore

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

    def generic_visit(self, node):
        self.simplevar = False
        super(ExprExaminer, self).generic_visit(node)
    

class ExprEvaluator(object):
    """A class that translates an expression string into a new string
    containing any necessary framework access functions, e.g., set, get. The
    compiled bytecode is stored within the object so that it doesn't have to
    be reparsed during later evaluations. A scoping object is required at
    construction time or evaluation time, and that object determines the form
    of the translated expression. Array entry access, 'downstream' attribute access, and
    function invocation are also translated in a similar way.  For a description
    of the format of the 'index' arg of set/get that is generated by ExprEvaluator,
    see the doc string for the ``openmdao.main.index.process_index_entry`` function.
    """
    
    def __init__(self, text, scope=None, getter='get'):
        self._scope = None
        self.scope = scope
        self.text = text
        self.getter = getter
        self.var_names = set()
    
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
                raise RuntimeError('ExprEvaluator scoping object no longer exists.')
            return scope
        return None
        
    @scope.setter
    def scope(self, value):
        if value is not self.scope:
            self._code = self._assignment_code = None
            self._examiner = self.cached_grad_eq = None
            if value is not None:
                self._scope = weakref.ref(value)
            else:
                self._scope = None
        
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
        """Returns True if this expression refers to the given variable or component."""
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
        if state.get('_assignment_code'):
            state['_assignment_code'] = None # more unpicklable <type 'code'>
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        self.__dict__.update(state)
        if self._scope is not None:
            self._scope = weakref.ref(self._scope)

    def is_local(self, name):
        """Return True if the given (dotted) name refers to something in our
        _expr_dict dict, e.g., math.sin.  Raises a KeyError if the name
        refers to something in _expr_dict that doesn't exist, e.g., math.foobar.
        Returns False if the name refers to nothing in _expr_dict, e.g., mycomp.x.
        """
        global _expr_dict
        if hasattr(self.scope, name):
            return False
        if hasattr(__builtin__, name) or name=='_local_setter_':
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
        
    def _pre_parse(self):
        try:
            root = ast.parse(self.text, mode='eval')
            if not isinstance(root.body, (ast.Attribute, ast.Name, ast.Subscript)):
                self._allow_set = False
            else:
                self._allow_set = True
        except SyntaxError:
            # might be an assignment, try mode='exec'
            root = ast.parse(self.text, mode='exec')
            self._allow_set = False
        return root
        
    def _parse_get(self):
        new_ast = ExprTransformer(self, getter=self.getter).visit(self._pre_parse())
        
        # compile the transformed AST
        ast.fix_missing_locations(new_ast)
        mode = 'exec' if isinstance(new_ast, ast.Module) else 'eval'
        return (new_ast, compile(new_ast, '<string>', mode))
        
    def _parse_set(self):
        root = ast.parse("%s=_local_setter_" % self.text, mode='exec')
        ## transform into a 'set' call to set the specified variable
        assign_ast = ExprTransformer(self, getter=self.getter).visit(root)
        ast.fix_missing_locations(assign_ast)
        code = compile(assign_ast,'<string>','exec')
        return (assign_ast, code)
    
    def _parse(self):
        self.var_names = set()
        try:
            new_ast, self._code = self._parse_get()
        except SyntaxError as err:
            raise SyntaxError("failed to parse expression '%s': %s" % (self.text, str(err)))
        
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
        global _expr_dict
        scope = self._get_updated_scope(scope)
        try:
            if self._code is None:
                self._parse()
            return eval(self._code, _expr_dict, locals())
        except Exception, err:
            raise type(err)("can't evaluate expression "+
                            "'%s': %s" %(self.text,str(err)))
        
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
    
    def evaluate_gradient(self, stepsize=1.0e-6, wrt=None, scope=None):
        """Return a dict containing the gradient of the expression with respect to 
        each of the referenced varpaths. The gradient is calculated by 1st order central
        difference for now. 
        
        stepsize: float
            Step size for finite difference.
            
        wrt: list of varpaths
            Varpaths for which we want to calculate the gradient.
        """
        global _expr_dict
        
        scope = self._get_updated_scope(scope)
        inputs = list(self.refs(copy=False))

        if wrt==None:
            wrt = inputs
        elif isinstance(wrt, str):
            wrt = [wrt]
                
        gradient = {}
        if self.cached_grad_eq is None:
            self.cached_grad_eq = {}

        for var in wrt:

            # A "fake" boundary connection in an assembly has a special
            # format. All expresion derivatives from inside the assembly are
            # handled outside the assembly.
            if var[0:4] == '@bin':
                gradient[var] = 1.0
                continue
            
            # Don't take derivative with respect to a variable that is not in
            # the expression
            if var not in inputs:
                gradient[var] = 0.0
                continue
            
            # First time, try to differentiate symbolically
            if (var not in self.cached_grad_eq) or self._code is None:
                
                #Take symbolic gradient of all inputs using sympy
                try:
                    for varname, expression in zip(inputs, SymGrad(self.text, inputs)):
                        self.cached_grad_eq[varname] = expression

                except SymbolicDerivativeError, NameError:
                    self.cached_grad_eq[var] = False

            # If we have a cached gradient expression:
            if self.cached_grad_eq[var]:
                
                # This is not the way I wanted to do it, but I didn't want
                # to mess with everything that's is in self._parse
                grad_text = self.cached_grad_eq[var]
                for name in inputs:
                    if '[' in name:
                        new_expr = ExprEvaluator(name, scope)
                        replace_val = new_expr.evaluate()
                    else:
                        replace_val = scope.get(name)
                    grad_text = grad_text.replace(name, str(replace_val))
                
                grad_root = ast.parse(grad_text, mode='eval')
                grad_code = compile(grad_root, '<string>', 'eval')
                gradient[var] = eval(grad_code, _expr_dict, locals())
                
            # Otherwise resort to finite difference (1st order central)
            else:
                # Always need to assemble list of constant inputs, for
                # replacement in the gradient expression text
                var_dict = {}
                grad_text = self.text
                for name in inputs:
                    if '[' in name:
                        new_expr = ExprEvaluator(name, scope)
                        replace_val = new_expr.evaluate()
                    else:
                        replace_val = scope.get(name)
                        
                    if name==var:
                        var_dict[name] = replace_val
                        new_name = "var_dict['%s']" % name
                        grad_text = grad_text.replace(name, new_name)
                    else:
                        # If we don't need derivative of a var, replace with its value
                        grad_text = grad_text.replace(name, str(replace_val))

                grad_root = ast.parse(grad_text, mode='eval')
                grad_code = compile(grad_root, '<string>', 'eval')

                # Finite difference (Central difference)
                var_dict[var] += 0.5*stepsize
                yp = eval(grad_code, _expr_dict, locals())
                var_dict[var] -= stepsize
                ym = eval(grad_code, _expr_dict, locals())
                    
                gradient[var] = (yp-ym)/stepsize
                
        return gradient
    
    def set(self, val, scope=None, src=None):
        """Set the value of the referenced object to the specified value."""
        global _expr_dict
        scope = self._get_updated_scope(scope)

        if self.is_valid_assignee():
            # self.assignment_code is a compiled version of an assignment statement
            # of the form  'somevar = _local_setter_', so we set _local_setter_ here
            # and the exec call will pull it out of the locals dict. _local_src_ is
            # another local variable corresponding to the 'src' arg which is used
            # to determine if a connected expression is being set by the source it's
            # connected to.
            _local_setter_ = val 
            _local_src_ = src
            if self._assignment_code is None:
                _, self._assignment_code = self._parse_set()
            exec(self._assignment_code, _expr_dict, locals())
        else:
            raise ValueError("expression '%s' can't be set to a value" % self.text)
        
    def get_metadata(self, metaname=None, scope=None):
        """Return the specified piece of metadata if metaname is provided. Otherwise
        return the whole metadata dictionary.  If metaname is supplied but does not
        exist for a given variable, None will be returned for the variable.
        
        Returns a list of tuples containing (varname, metadata) 
        corresponding to each variable referenced by this expression.
        """
        scope = self._get_updated_scope(scope)
        return [(name, scope.get_metadata(name, metaname)) 
                  for name in self.get_referenced_varpaths(copy=False)]

    def get_referenced_varpaths(self, copy=True):
        """Return a set of pathnames relative to *scope.parent* and based on
        the names of Variables referenced in our expression string.
        """
        if self._code is None:
            self._parse()
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
            parts = name.split('.',1)
            if len(parts) > 1:
                nameset.add(parts[0])
        return nameset
    
    def get_required_compnames(self, assembly):
        """Return the set of all names of Components that evaluation
        of our expression string depends on, either directly or indirectly.
        """
        compset = self.get_referenced_compnames()
        graph = assembly._depgraph.copy_graph()
        visited = set()
        while compset:
            comp = compset.pop()
            if comp not in visited:
                visited.add(comp)
                diff = set(graph.predecessors(comp)).difference(visited)
                compset.update(diff)
        return visited
    
    def refs_valid(self):
        """Return True if all variables referenced by our expression
        are valid.
        """
        if self.scope:
            if self._code is None:
                self._parse()
            if not all(self.scope.get_valid(self.var_names)):
                return False
        return True
    
    def refs_parent(self):
        """Return True if this expression references a variable in parent."""
        if self._code is None:
            self._parse()
        for name in self.var_names:
            if name.startswith('parent.'):
                return True
        return False

    def invalid_refs(self):
        """Return a list of invalid variables referenced by this expression."""
        if self._code is None:
            self._parse()
        valids = self.scope.get_valid(self.var_names)
        return [n for n,v in zip(self.var_names, valids) if v is False]
    
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
        """Return a transformed version of our text string where the attribute names are
        changed based on a change in scope to the given object.
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
            raise SyntaxError("failed to transform expression '%s': %s" % (self.text, str(err)))
    
    def __eq__(self,other):
        if isinstance(other,self.__class__): 
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
        if len(self._examiner.refs) != 1:
            raise RuntimeError("bad connected expression '%s' must reference exactly one variable" %
                               self.text)
        if self._is_dest:
            if not self._examiner.const_indices:
                raise RuntimeError("bad destination expression '%s': only constant indices are allowed for arrays and slices" %
                                   self.text)
            if not self._examiner.assignable:
                raise RuntimeError("bad destination expression '%s': not assignable" %
                                   self.text)
    
    def refers_to(self, name):
        """Returns True if this expression refers to the given variable or component."""
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
    
    
