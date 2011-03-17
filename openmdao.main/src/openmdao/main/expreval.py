# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]

import weakref
import math
import ast


class _Globals(object):
    def __init__(self):
        self.math = math
        self._local_setter = None
        for name in dir(math):
            if not name.startswith('_'):
                setattr(self, name, getattr(math, name))
        
_globals_scope = _Globals()

def _in_globals(name):
    """Return True if the given (dotted) name refers to something in our
    globals object, e.g., math.sin.  Raises an AttributeError if the name
    refers to something in globals that doesn't exist, e.g., math.foobar.
    Returns False if the name refers to nothing in globals, e.g., mycomp.x.
    """
    obj = _globals_scope
    for i,part in enumerate(name.split('.')):
        obj = getattr(obj, part, None)
        if obj is None:
            if i==0:
                return False  # object is nowhere in globals
            else: # part of name refers to a globals object, but full dotted 
                  # path doesn't point to an existing object
                raise AttributeError("Can't find '%s' in globals" % name)
    return True

def _get_name_node(names):
    node = ast.Name(id=names[0], ctx=ast.Load())
    for name in names[1:]:
        node = ast.Attribute(value=node, attr=name, ctx=ast.Load())
    return node


def _cannot_find(name):
    raise RuntimeError("ExprEvaluator: cannot find variable '%s'" % name)


#_allowed_nodes = (ast.BoolOp, ast.BinOp, ast.UnaryOp, ast.Attribute, ast.Assign, 
                  #ast.Name, ast.IfExp, ast.Call, ast.Num, 
                  #ast.Str, ast.Subscript, ast.Index, ast.Slice, ast.List, ast.Tuple, 
                  #ast.USub, ast.UAdd, ast.And, ast.Or,
                  #ast.Add, ast.Sub, ast.Mult, ast.Div, ast.Mod, ast.Pow, 
                  #ast.LShift, ast.RShift, ast.BitOr, ast.BitXor, ast.BitAnd, 
                  #ast.FloorDiv, ast.Eq, ast.NotEq, ast.Lt, ast.LtE, ast.Gt, ast.GtE, 
                  #ast.Is, ast.IsNot, ast.In, ast.NotIn
                  #)
                  
                  
class ExprPrinter(ast.NodeVisitor):
    def __init__(self):
        super(ExprPrinter, self).__init__()
        self.txtlist = []
        
    def write(self, txt):
        self.txtlist.append(txt)
        
    def get_text(self):
        return ''.join(self.txtlist)

    #def generic_visit(self, node):
        #if not isinstance(node, _allowed_nodes):
            #raise TypeError("Expression AST contains a node of type %s which is not allowed." %
                            #node.__class__.__name__)
        #super(ExprPrinter, self).generic_visit(node)
        
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
        self.visit(node.lower)
        self.write(':')
        self.visit(node.upper)
        self.write(':')
        if node.step is not None:
            self.visit(node.step)
        
    def visit_List(self, node):
        self.write('[')
        for i,e in enumerate(node.elts):
            if i>0:
                self.write(',')
            self.visit(e)
        self.write(']')
        
    def visit_Tuple(self, node):
        self.write('(')
        for i,e in enumerate(node.elts):
            if i>0:
                self.write(',')
            self.visit(e)
        if len(node.elts) == 1:
            self.write(',')
        self.write(')')
        
    def visit_USub(self, node):
        self.write('-')
        
    def visit_UAdd(self, node):
        self.write('+')
        
    def visit_And(self, node):
        self.write(' and ')
        
    def visit_Or(self, node):
        self.write(' or ')
        
    # operators
    #Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
    def visit_Add(self, node):
        self.write('+')
        
    def visit_Sub(self, node):
        self.write('-')
        
    def visit_Mult(self, node):
        self.write('*')
        
    def visit_Div(self, node):
        self.write('/')
        
    def visit_Mod(self, node):
        self.write('%')
        
    def visit_Pow(self, node):
        self.write('**')
        
    def visit_LShift(self, node):
        self.write('<<')
        
    def visit_Rshift(self, node):
        self.write('>>')
        
    def visit_BitOr(self, node):
        self.write('|')
        
    def visit_BitXor(self, node):
        self.write('^')
        
    def visit_BitAnd(self, node):
        self.write('&')
        
    def visit_FloorDiv(self, node):
        self.write('//')
        
    # cmp operators
    # Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
    def visit_Eq(self, node):
        self.write('==')

    def visit_NotEq(self, node):
        self.write('!=')

    def visit_Lt(self, node):
        self.write('<')

    def visit_LtE(self, node):
        self.write('<=')

    def visit_Gt(self, node):
        self.write('>')

    def visit_GtE(self, node):
        self.write('>=')

    def visit_Is(self, node):
        self.write(' is ')

    def visit_IsNot(self, node):
        self.write(' is not ')

    def visit_In(self, node):
        self.write(' in ')

    def visit_NotIn(self, node):
        self.write(' not in ')

        
class ExprTransformer(ast.NodeTransformer):
    """Transforms attribute accesses, e.g., abc.def.g in an expression into 
    scope.get('abc.def.g') or scope.parent.get('abc.def.g')
    """
    def __init__(self, expreval, rhs=None):
        self.expreval = expreval
        self.rhs = rhs
        self._stack = []  # use this to see if we're inside of parens or brackets so
                          # that we always translate to 'get' even if we're on the lhs
        super(ExprTransformer, self).__init__()
        
    def _get_long_name(self, node):
        # if node is an Attribute composed of only other Attributes and a Name
        # then return the full dotted name. Otherwise, return None
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
    
    def visit_Attribute(self, node, subs=None):
        long_name = self._get_long_name(node)
        if long_name is None:
            return node
        
        if _in_globals(long_name):
            return node
    
        args = [ast.Str(s=long_name)]
        scope = self.expreval.scope
        names = []
        if scope:
            if scope.contains(long_name.split('.',1)[0]):
                names = ['scope']
            else:
                names = ['scope', 'parent']
        else:
            raise RuntimeError("expression '%s' can't be evaluated because it has no scope" % str(self.expreval))

        if self.rhs and len(self._stack) == 0:
            fname = 'set'
        else:
            fname = 'get'
        names.append(fname)
        if fname == 'set':
            args.append(self.rhs)

        called_obj = _get_name_node(names)
        if subs:
            args.append(ast.List(elts=subs))

        return ast.copy_location(ast.Call(func=called_obj, args=args,
                                          ctx=node.ctx), node)

    def visit_Subscript(self, node, sublist=None):
        self._stack.append(node)
        if sublist:
            subs = [self.visit(node.slice.value)] + sublist
        else:
            subs = [self.visit(node.slice.value)]
        self._stack.pop()
            
        if isinstance(node.value, ast.Attribute):
            node = self.visit_Attribute(node.value, subs)
        elif isinstance(node.value, ast.Subscript):
            node = self.visit_Subscript(node.value, subs)
        return node
    
    def visit_Module(self, node):
        # Make sure there is only one statement or expression
        if len(node.body) > 1 or not isinstance(node.body[0], (ast.Assign, ast.Expr)):
            raise RuntimeError("Only one assignment statement or expression is allowed")
        return super(ExprTransformer, self).generic_visit(node)
        
    def visit_Assign(self, node):
        if len(node.targets) > 1:
            raise RuntimeError("only one expression is allowed on left hand side of assignment")
        return ExprTransformer(self.expreval, rhs=self.visit(node.value)).visit(node.targets[0])
    
class ExprEvaluator(str):
    """A class that translates an expression string into a new string containing
    any necessary framework access functions, e.g., set, get. The compiled
    bytecode is stored within the object so that it doesn't have to be reparsed
    during later evaluations.  A scoping object is required at construction time,
    and that object determines the form of the  translated expression based on scope. 
    Variables that are local to the scoping object do not need to be translated,
    whereas variables from other objects must  be accessed using the appropriate
    *set()* or *get()* call.  Array entry access and function invocation are also
    translated in a similar way.  For example, the expression "a+b[2]-comp.y(x)"
    for a scoping object that contains variables a and b, but not comp,x or y,
    would translate to 
    "a+b[2]-self.parent.invoke('comp.y',self.parent.get('x'))".
    
    If *lazy_check* is False, any objects referenced in the expression must exist
    at creation time (or any time later that text is set to a different value)
    or a RuntimeError will be raised.  If *lazy_check* is True, error reporting will
    be delayed until the expression is evaluated.
    
    If *single_name* is True, the expression can only be the name of one object, with
    optional array indexing, but general expressions are not allowed because the
    expression is intended to be on the LHS of an assignment statement.
    """
    
    def __new__(cls, text, scope=None, single_name=False, lazy_check=True):
        s = super(ExprEvaluator, cls).__new__(ExprEvaluator, text)
        s.scope = scope
        s.lazy_check = lazy_check
        s.single_name = single_name
        s._text = None  # used to detect change in str
        s.var_names = set()
        s._has_globals = False
        if lazy_check is False:
            s._parse()
        return s
    
    @property
    def scope(self):
        if self._scope:
            scope = self._scope()
            if scope is None:
                raise RuntimeError('ExprEvaluator scoping object does not exist.')
            return scope
        return None
        
    @scope.setter
    def scope(self, value):
        if value is not None:
            self._scope = weakref.ref(value)
        else:
            self._scope = None

    def __getstate__(self):
        """Return dict representing this container's state."""
        state = self.__dict__.copy()
        if state['_scope'] is not None:
            # remove weakref to scope because it won't pickle
            state['_scope'] = self._scope()
        state['_code'] = None  # <type 'code'> won't pickle either.
        if state.get('_assignment_code'):
            state['_assignment_code'] = None # more unpicklable <type 'code'>
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        self.__dict__.update(state)
        if self._scope is not None:
            self._scope = weakref.ref(self._scope)
        if self.scoped_text:
            self._code = compile(self.scoped_text, '<string>', 'eval')
        satxt = state.get('scoped_assignment_text')
        if satxt:
            self._assignment_code = compile(self.scoped_assignment_text, 
                                            '<string>', 'exec')

    def _parse(self):
        self.var_names = set()
        root = ast.parse(self, mode='exec')
        
        # transform attribute accesses to 'get' calls if necessary
        new_ast = ExprTransformer(self).visit(root)
        
        ## now take the new AST and save it to a string (for debugging purposes)
        ep = ExprPrinter()
        ep.visit(new_ast)
        self.scoped_text = ep.get_text()
        
        # compile the transformed AST
        self._code = compile(self.scoped_text, '<string>', 'eval')
        
        if self.single_name: # set up a compiled assignment statement
            old_lazy_check = self.lazy_check
            try:
                self.lazy_check = True
                assign_txt = "%s=_local_setter" % self
                root = ast.parse(assign_txt, mode='exec')
                # transform into a 'set' call to set the specified variable
                new_ast = ExprTransformer(self).visit(root)
                ep = ExprPrinter()
                ep.visit(new_ast)
                self._assignment_code = compile(ep.get_text(),'<string>','eval')
            finally:
                self.lazy_check = old_lazy_check
                

    def evaluate(self):
        """Return the value of the scoped string, evaluated 
        using the eval() function.
        """
        scope = self.scope
        try:
            if self._text != self:  # text has changed
                self._parse()
            if scope:
                if self._has_globals:
                    globdict = _evalglobals.copy()
                    globdict.update(scope.__dict__)
                else:
                    globdict = scope.__dict__
                return eval(self._code, globdict, locals())
            else:
                return eval(self._code, _evalglobals, locals())
        except Exception, err:
            raise type(err)("ExprEvaluator failed evaluating expression "+
                            "'%s'. Caught message is: %s" %(self,str(err)))

    def set(self, val):
        """Set the value of the referenced object to the specified value."""
        scope = self.scope
        if scope is None:
            raise RuntimeError(
                'ExprEvaluator cannot evaluate expression without scope.')
        
        if self.single_name:           
            # self.assignment_code is a compiled version of an assignment statement
            # of the form  'somevar = _local_setter', so we set _local_setter here
            # and the exec call will pull it out of locals()
            _local_setter = val 
            if self._text != self:  # text has changed
                self._parse()
            exec(self._assignment_code, scope.__dict__, locals())
        else: # self.single_name is False
            raise ValueError("trying to set input expression '%s'" % str(self))
        
    def get_referenced_varpaths(self):
        """Return a set of source or dest Variable pathnames relative to
        *self.parent* and based on the names of Variables referenced in our 
        reference string. 
        """
        if self._text != self:  # text has changed
            self._parse()
        return self.var_names

    def get_referenced_compnames(self):
        """Return a set of source or dest Component names based on the 
        pathnames of Variables referenced in our reference string. 
        """
        if self._text != self:  # text has changed
            self._parse()
        return set([x.split('.')[0] for x in self.var_names])
    
    def refs_valid(self):
        """Return True if all variables referenced by our expression
        are valid.
        """
        if self.single_name:
            return True   # since we're setting this expression, we don't care if it's valid
        if self._scope:
            scope = self._scope()
            if scope and scope.parent:
                if self._text != self:  # text has changed
                    self._parse()
                if not all(scope.parent.get_valid(self.var_names)):
                    return False
        return True
    
    def check_resolve(self):
        """Return True if all variables referenced by our expression can
        be resolved.
        """
        if self._scope:
            scope = self._scope()
            if scope and scope.parent:
                if self._text != self:  # text has changed
                    self._parse()
                if scope.parent.check_resolve(self.var_names):
                    return True
        return False


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
            'b': {
                'c': 1,
                'd': 2,
                }
            },
        'x': {
            'y': {
                'z': 3.14,
                }
            }
        })
    
    root = ExprTransformer(ExprEvaluator(txt, scope=top)).visit(root)
    print '\ntransformed AST dump:'
    print ast.dump(root, annotate_fields=True)
    
    print '\nprinted transformed AST:'
    ep = ExprPrinter()
    ep.visit(root)
    print ep.get_text()
    

