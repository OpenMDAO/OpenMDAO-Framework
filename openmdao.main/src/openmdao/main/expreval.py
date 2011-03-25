# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]

import weakref
import math
import ast
import __builtin__

# a copy of this dict will act as the local scope when we eval our expressions
_expr_dict = {
    'math': math,
    }
# add stuff from math lib directly to our locals dict so users won't have to 
# put 'math.' in front of all of their calls to standard math functions 
for name in dir(math):
    if not name.startswith('_'):
        _expr_dict[name] = getattr(math, name)

_Missing = object()


# some constants used in the get/set downstream protocol
INDEX = 0
ATTR = 1
CALL = 2
SLICE = 3


def _get_attr_node(names):
    """Builds an Attribute node, or a Name node if names has just one entry."""
    node = ast.Name(id=names[0], ctx=ast.Load())
    for name in names[1:]:
        node = ast.Attribute(value=node, attr=name, ctx=ast.Load())
    return node


# dict with operator precedence.  We need this because otherwise we can't
# tell where to put parens when we print out an expression with mixed operators.  
# We could just put them around every operation, but that's a little ugly...
_op_preds = {}
_prec = 0

_op_preds[ast.Lambda] = _prec
_prec += 1
_op_preds[ast.If] = _prec
_prec += 1
_op_preds[ast.Or] = _prec
_prec += 1
_op_preds[ast.And] = _prec
_prec += 1
_op_preds[ast.Not] = _prec
_prec += 1
_op_preds[ast.In] = _prec
_op_preds[ast.NotIn] = _prec
_op_preds[ast.Is] = _prec
_op_preds[ast.IsNot] = _prec
_op_preds[ast.Lt] = _prec
_op_preds[ast.LtE] = _prec
_op_preds[ast.Gt] = _prec
_op_preds[ast.GtE] = _prec
_op_preds[ast.NotEq] = _prec
_op_preds[ast.Eq] = _prec
_prec += 1
_op_preds[ast.BitOr] = _prec
_prec += 1
_op_preds[ast.BitXor] = _prec
_prec += 1
_op_preds[ast.BitAnd] = _prec
_prec += 1
_op_preds[ast.LShift] = _prec
_op_preds[ast.RShift] = _prec
_prec += 1
_op_preds[ast.Add] = _prec
_op_preds[ast.Sub] = _prec
_prec += 1
_op_preds[ast.Mult] = _prec
_op_preds[ast.Div] = _prec
_op_preds[ast.FloorDiv] = _prec
_op_preds[ast.Mod] = _prec
_prec += 1
_op_preds[ast.UAdd] = _prec
_op_preds[ast.USub] = _prec
_op_preds[ast.Invert] = _prec
_prec += 1
_op_preds[ast.Pow] = _prec

def _pred_cmp(op1, op2):
    return _op_preds[op1.__class__] - _op_preds[op2.__class__]

class ExprPrinter(ast.NodeVisitor):
    """A NodeVisitor that gets the text of the expression defined by the AST."""
    def __init__(self):
        super(ExprPrinter, self).__init__()
        self.txtlist = []
        
    def write(self, txt):
        self.txtlist.append(txt)
        
    def get_text(self):
        return ''.join(self.txtlist)

    def visit_Attribute(self, node):
        self.visit(node.value)
        self.write(".%s" % node.attr)
        
    def visit_Assign(self, node):
        for i,t in enumerate(node.targets):
            if i>0: self.write(',')
            self.visit(t)
        self.write(' = ')
        self.visit(node.value)
        
    def visit_Name(self, node):
        self.write(node.id)
        
    def visit_BinOp(self, node):
        # we have to add parens around any immediate BinOp child
        # that has a lower precedence operation than we do
        if isinstance(node.left, ast.BinOp) and _pred_cmp(node.left.op, node.op) < 0:
            self.write('(')
            self.visit(node.left)
            self.write(')')
        else:
            self.visit(node.left)
        self.visit(node.op)
        if isinstance(node.right, ast.BinOp) and _pred_cmp(node.right.op, node.op) < 0:
            self.write('(')
            self.visit(node.right)
            self.write(')')
        else:
            self.visit(node.right)

    def visit_IfExp(self, node):
        self.visit(node.body)
        self.write(' if ')
        self.visit(node.test)
        self.write(' else ')
        self.visit(node.orelse)
        
    def visit_Call(self, node):
        self.visit(node.func)
        self.write('(')
        total_args = 0
        for arg in node.args:
            if total_args>0: self.write(',')
            self.visit(arg)
            total_args += 1
            
        if hasattr(node, 'keywords'):
            for kw in node.keywords:
                if total_args>0: self.write(',')
                self.visit(kw)
                total_args += 1
            
        if hasattr(node, 'starargs'):
            if node.starargs:
                if total_args>0: self.write(',')
                self.write('*%s'%node.starargs)
                total_args += 1
            
        if hasattr(node, 'kwargs'):
            if node.kwargs:
                if total_args>0: self.write(',')
                self.write('**%s'%node.kwargs)
    
        self.write(')')
        
    def visit_Num(self, node):
        self.write(str(node.n))
        
    def visit_Str(self, node):
        self.write("'%s'" % node.s)
        
    def visit_Index(self, node):
        self.write('[')
        self.visit(node.value)
        self.write(']')

    def visit_Slice(self, node):
        self.write('[')
        if node.lower is not None:
            self.visit(node.lower)
        self.write(':')
        if node.upper is not None:
            self.visit(node.upper)
        self.write(':')
        if node.step is not None:
            self.visit(node.step)
        self.write(']')
        
    def visit_List(self, node):  
        self.write('[')
        for i,e in enumerate(node.elts):
            if i>0: self.write(',')
            self.visit(e)
        self.write(']')
        
    def visit_Dict(self, node):  
        self.write('{')
        for i,tup in enumerate(zip(node.keys,node.values)):
            if i>0: self.write(',')
            self.write("'%s':" % tup[0].s)
            self.visit(tup[1])
        self.write('}')
        
    def visit_Tuple(self, node): 
        self.write('(')
        length = len(node.elts)
        for i,e in enumerate(node.elts):
            if i>0: self.write(',')
            self.visit(e)
        if length==1: self.write(',')
        self.write(')')
        
    def visit_USub(self, node): self.write('-')
    def visit_UAdd(self, node): self.write('+')
    def visit_And(self, node):  self.write(' and ')
    def visit_Or(self, node):   self.write(' or ')
        
    # operators
    def visit_Add(self, node):      self.write('+')
    def visit_Sub(self, node):      self.write('-')
    def visit_Mult(self, node):     self.write('*')
    def visit_Div(self, node):      self.write('/')
    def visit_Mod(self, node):      self.write('%')
    def visit_Pow(self, node):      self.write('**')
    def visit_LShift(self, node):   self.write('<<')
    def visit_Rshift(self, node):   self.write('>>')
    def visit_BitOr(self, node):    self.write('|')
    def visit_BitXor(self, node):   self.write('^')
    def visit_BitAnd(self, node):   self.write('&')
    def visit_FloorDiv(self, node): self.write('//')
        
    # cmp operators
    def visit_Eq(self, node):    self.write('==')
    def visit_NotEq(self, node): self.write('!=')
    def visit_Lt(self, node):    self.write('<')
    def visit_LtE(self, node):   self.write('<=')
    def visit_Gt(self, node):    self.write('>')
    def visit_GtE(self, node):   self.write('>=')
    def visit_Is(self, node):    self.write(' is ')
    def visit_IsNot(self, node): self.write(' is not ')
    def visit_In(self, node):    self.write(' in ')
    def visit_NotIn(self, node): self.write(' not in ')
    
    def _ignore(self, node):
        super(ExprPrinter, self).generic_visit(node)

    visit_Module    = _ignore
    visit_Expr      = _ignore
    visit_Compare   = _ignore
    visit_UnaryOp   = _ignore
    visit_Subscript = _ignore
    visit_Load      = _ignore
    visit_Store     = _ignore
    
    def generic_visit(self, node):
        # We want to fail if we see any nodes we don't know about rather than
        # generating code that isn't correct.
        raise RuntimeError("ExprPrinter can't handle a node of type %s" % node.__class__.__name__)

        
class ExprTransformer(ast.NodeTransformer):
    """Transforms dotted name references, e.g., abc.d.g in an expression AST into 
    scope.get('abc.d.g') or scope.parent.get('abc.d.g'). Also turns assignments
    into the appropriate set() calls.  Also translates function calls and indirect
    attribute accesses into a form that can be passed to a downstream object and
    executed there.  For example, abc.d[xyz](1, pdq-10).value would translate to, e.g.,
    scope.get('abc.d', [(0,xyz), (0,[1,pdq-10]), (1,'value')]).
    """
    def __init__(self, expreval, rhs=None):
        self.expreval = expreval
        self.rhs = rhs
        self._stack = []  # use this to see if we're inside of parens or brackets so
                          # that we always translate to 'get' even if we're on the lhs
        super(ExprTransformer, self).__init__()
        
    def visit(self, node, subs=None):
        """Visit a node."""
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        if visitor == self.generic_visit:
            return visitor(node)
        else:
            return visitor(node, subs)

    def _get_long_name(self, node):
        # If this node is an Attribute or Name node that is composed
        # only of other Attribute or Name nodes, then return the full
        # dotted name for this node. Otherwise, i.e., if this node
        # contains Subscripts or Calls, return None.
        if isinstance(node, ast.Name):
            return node.id
        elif not isinstance(node, ast.Attribute):
            return None
        val = node.value
        parts = [node.attr]
        while True:
            if isinstance(val, ast.Attribute):
                parts.append(val.attr)
                val = val.value
            elif isinstance(val, ast.Name):
                parts.append(val.id)
                break
            else:  # it's more than just a simple dotted name
                return None
        return '.'.join(parts[::-1])
    
    def _name_to_node(self, node, name, subs=None):
        """Given a dotted name, return the proper node depending on whether
        the name is resolvable in 'local' scope or not.
        """
        if name is None:
            return super(ExprTransformer, self).generic_visit(node)
        
        if self.expreval._is_local(name):
            return node
        
        scope = self.expreval.scope
        if scope:
            parts = name.split('.',1)
            names = ['scope']
            if scope.contains(parts[0]):
                self.expreval.var_names.add('.'.join([scope.name,name]))
                if len(parts) == 1: # short name, so just do a simple attr lookup on scope
                    names.append(name)
                    return _get_attr_node(names)
            else:
                self.expreval.var_names.add(name)
                if scope.parent is not None:
                    names.append('parent')
        else:
            raise RuntimeError("expression has no scope")

        if self.rhs and len(self._stack) == 0:
            fname = 'set'
        else:
            fname = 'get'
        names.append(fname)
        args = [ast.Str(s=name)]
        if fname == 'set':
            args.append(self.rhs)

        called_obj = _get_attr_node(names)
        if subs:
            args.append(ast.List(elts=subs, ctx=ast.Load()))

        return ast.copy_location(ast.Call(func=called_obj, args=args,
                                          ctx=node.ctx, keywords=[]), node)
    
    def visit_Name(self, node, subs=None):
        return self._name_to_node(node, node.id, subs)
    
    def visit_Attribute(self, node, subs=None):
        long_name = self._get_long_name(node)
        if long_name is None: # this Attribute contains more than just names/attrs
            if subs is None:
                subs = []
            subs[0:0] = [ast.Tuple(elts=[ast.Num(n=ATTR),ast.Str(s=node.attr)], ctx=ast.Load())]
            return self.visit(node.value, subs)
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
        name = self._get_long_name(node.func)
        if name is not None:
            if self.expreval._is_local(name):
                return self.generic_visit(node)
            elif isinstance(node.func, ast.Name):
                self._stack.append(node)
                kwargs = {
                    'func': _get_attr_node(['scope',node.func.id]),
                    'args': [self.visit(arg) for arg in node.args],
                    'keywords': [(kw.arg,self.visit(kw.value)) for kw in node.keywords],
                    'ctx': node.func.ctx,
                    }
                self._stack.pop()
                if hasattr(node, 'kwargs') and node.kwargs:
                    kwargs['kwargs'] = node.kwargs
                if hasattr(node, 'starargs') and node.starargs:
                    kwargs['starargs'] = node.starargs
                return ast.copy_location(ast.Call(**kwargs), node)
        
        if subs is None:
            subs = []
        
        self._stack.append(node)
        
        call_list = []
        
        if hasattr(node, 'kwargs') and node.kwargs:
            call_list.append(node.kwargs)
            
        if hasattr(node, 'starargs') and node.starargs:
            call_list.append(node.starargs)
        elif len(call_list) > 0:
            call_list.append(ast.Name(id='None', ctx=ast.Load()))
            
        if hasattr(node, 'keywords') and len(node.keywords) > 0:
            call_list.append(ast.Dict(keys=[ast.Str(kw.arg) for kw in node.keywords],
                                values=[self.visit(kw.value) for kw in node.keywords],
                                ctx=ast.Load()))
        elif len(call_list) > 0:
            call_list.append(ast.Dict(keys=[], values=[], ctx=ast.Load()))

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
        if len(node.body) > 1 or not isinstance(node.body[0], (ast.Assign, ast.Expr)):
            raise RuntimeError("Only one assignment statement or expression is allowed")
        return super(ExprTransformer, self).generic_visit(node)
    
    def visit_Assign(self, node, subs=None):
        if len(node.targets) > 1:
            raise RuntimeError("only one expression is allowed on left hand side of assignment")
        return ExprTransformer(self.expreval, rhs=self.visit(node.value)).visit(node.targets[0])


class ExprEvaluator(object):
    """A class that translates an expression string into a new string
    containing any necessary framework access functions, e.g., set, get. The
    compiled bytecode is stored within the object so that it doesn't have to
    be reparsed during later evaluations. A scoping object is required at
    construction time or evaluation time, and that object determines the form
    of the translated expression. Variables that are local to the scoping
    object are translated to a simple attribute access on the object, whereas
    variables from other objects must be accessed using the appropriate
    *set()* or *get()* call. Array entry access, 'late' attribute access, and
    function invocation are also translated in a similar way.".
    """
    
    def __init__(self, text, scope=None):
        self._scope = None
        self.scope = scope
        self._allow_set = False
        self.text = text
        self.var_names = set()
    
    @property
    def text(self):
        return self._text
    
    @text.setter
    def text(self, value):
        self._parse_needed = True
        self._text = value

    @property
    def transformed_text(self):
        return self._transform_ast()[1]

    @property
    def scope(self):
        if self._scope:
            scope = self._scope()
            if scope is None:
                raise RuntimeError('ExprEvaluator scoping object no longer exists.')
            return scope
        return None
        
    @scope.setter
    def scope(self, value):
        if value is not self.scope:
            self._parse_needed = True
            if value is not None:
                self._scope = weakref.ref(value)
            else:
                self._scope = None
        
    def is_valid_assignee(self):
        """Returns True if the syntax of our expression is valid to
        be on the left hand side of an assignment.  No check is 
        performed to see if the variable(s) in the expression actually
        exist.
        """
        if self._parse_needed:
            self._pre_parse()
        return self._allow_set

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
        self._parse_needed = True  # force a reparse

    def _is_local(self, name):
        """Return True if the given (dotted) name refers to something in our
        _expr_dict dict, e.g., math.sin.  Raises a KeyError if the name
        refers to something in _expr_dict that doesn't exist, e.g., math.foobar.
        Returns False if the name refers to nothing in _expr_dict, e.g., mycomp.x.
        """
        global _expr_dict
        if hasattr(__builtin__, name) or name=='_local_setter':
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
        root = ast.parse(self.text, mode='exec')
        if isinstance(root.body[0], ast.Expr):
            topnode = root.body[0].value
        else:
            topnode = root.body[0]
        if not isinstance(topnode, (ast.Attribute, ast.Name, ast.Subscript)):
            self._allow_set = False
        else:
            self._allow_set = True
        return root
        
    def _transform_ast(self):
        """Returns a tuple of the form (new_AST, new_text) where new_AST
        is the transformed AST and new_text is the text of the transformed
        AST.
        """
        # transform attribute accesses to 'get' calls if necessary
        new_ast = ExprTransformer(self).visit(self._pre_parse())
        
        ## now take the new AST and save it to a string
        ep = ExprPrinter()
        ep.visit(new_ast)
        return (new_ast, ep.get_text())
        
    def _parse(self):
        self._allow_set = True
        self.var_names = set()
        new_ast, scoped_text = self._transform_ast()
        
        # compile the transformed AST
        # FIXME: we really want to just compile the transformed AST, but for
        #        now that still has problems...
        #ast.fix_missing_locations(new_ast)
        #self._code = compile(new_ast, '<string>', 'exec')
        self._code = compile(scoped_text, '<string>', 'eval')
        
        if self._allow_set: # set up a compiled assignment statement
            assign_txt = "%s=_local_setter" % self.text
            root = ast.parse(assign_txt, mode='exec')
            # transform into a 'set' call to set the specified variable
            new_ast = ExprTransformer(self).visit(root)
            # FIXME: we really want to just compile the transformed AST, but for
            #        now that still has problems...
            #ast.fix_missing_locations(new_ast)
            #self._assignment_code = compile(new_ast,'<string>','exec')
            ep = ExprPrinter()
            ep.visit(new_ast)
            self._assignment_code = compile(ep.get_text(),'<string>','eval')
            
        self._parse_needed = False
                
    def _get_updated_scope(self, scope):
        oldscope = self.scope
        if scope is None:
            scope = oldscope
        elif scope is not oldscope:
            self._parse_needed = True
            self.scope = scope
        return scope

    def evaluate(self, scope=None):
        """Return the value of the scoped string, evaluated 
        using the eval() function.
        """
        global _expr_dict
        scope = self._get_updated_scope(scope)
        try:
            if self._parse_needed:
                self._parse()
            return eval(self._code, _expr_dict, locals())
        except Exception, err:
            raise type(err)("can't evaluate expression "+
                            "'%s': %s" %(self.text,str(err)))

    def set(self, val, scope=None):
        """Set the value of the referenced object to the specified value."""
        global _expr_dict
        scope = self._get_updated_scope(scope)
        
        if self.is_valid_assignee():
            # self.assignment_code is a compiled version of an assignment statement
            # of the form  'somevar = _local_setter', so we set _local_setter here
            # and the exec call will pull it out of the locals dict
            _local_setter = val 
            if self._parse_needed:
                self._parse()
            exec(self._assignment_code, _expr_dict, locals())
        else: # self._allow_set is False
            raise ValueError("expression '%s' can't be set to a value" % self.text)
        
    def get_referenced_varpaths(self):
        """Return a set of source or dest Variable pathnames relative to
        *scope.parent* and based on the names of Variables referenced in our 
        expression string. 
        """
        if self._parse_needed:
            self._parse()
        return self.var_names

    def get_referenced_compnames(self):
        """Return a set of source or dest Component names based on the 
        pathnames of Variables referenced in our expression string. 
        """
        if self._parse_needed:
            self._parse()
        names = set()
        for x in self.var_names:
            parts = x.split('.')
            if len(parts) > 1:
                names.add(parts[0])
        return names
    
    def refs_valid(self):
        """Return True if all variables referenced by our expression
        are valid.
        """
        scope = self.scope
        if scope and scope.parent:
            if self._parse_needed:
                self._parse()
            if not all(scope.parent.get_valid(self.var_names)):
                return False
        return True
    
    def check_resolve(self):
        """Return True if all variables referenced by our expression can
        be resolved.
        """
        if self._parse_needed:
            self._parse()
        if len(self.var_names) > 0:
            scope = self.scope
            if scope:
                if scope.parent:
                    sc = scope.parent
                else:
                    sc = scope
                for name in self.var_names:
                    if not sc.contains(name):
                        return False
                return True
            return False
        return True


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
    
    print '\nvars referenced: %s' % expreval.get_referenced_varpaths()
    
    print '\nattempting to compile the transformed AST...'
    ast.fix_missing_locations(root)
    code = compile(root, '<string>', 'exec')
    
    
