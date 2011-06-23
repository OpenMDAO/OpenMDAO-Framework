# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]

import weakref
import math
import ast
import copy
import __builtin__

# this dict will act as the local scope when we eval our expressions
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

class ExprTransformer(ast.NodeTransformer):
    """Transforms dotted name references, e.g., abc.d.g in an expression AST
    into scope.get('abc.d.g') and turns assignments into the appropriate
    set() calls. Also, translates function calls and indirect attribute
    accesses into a form that can be passed to a downstream object and
    executed there. For example, abc.d[xyz](1, pdq-10).value would translate
    to, e.g., scope.get('abc.d', [(0,xyz), (0,[1,pdq-10]), (1,'value')]).
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
                #if scope.name:
                    #self.expreval.var_names.add('.'.join([scope.name,name]))
                #else:
                self.expreval.var_names.add(name)
                if len(parts) == 1: # short name, so just do a simple attr lookup on scope
                    names.append(name)
                    return _get_attr_node(names)
            else:
                self.expreval.var_names.add(name)
                #if scope.parent is not None:
                    #names.append('parent')
        else:
            raise RuntimeError("expression has no scope")

        args = [ast.Str(s=name)]
        if self.rhs and len(self._stack) == 0:
            fname = 'set'
            args.append(self.rhs)
        else:
            fname = 'get'
        names.append(fname)

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
        name = self._get_long_name(node.func)
        if name is not None:
            if self.expreval._is_local(name) or '.' not in name:
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
    function invocation are also translated in a similar way.  For a description
    of the format of the 'index' arg of set/get that is generated by ExprEvaluator,
    see the doc string for the ``Container._process_index_entry`` function.
    """
    
    def __init__(self, text, scope=None):
        self._scope = None
        self.scope = scope
        self._allow_set = False
        self.text = text
        self.var_names = set()
    
    @property
    def text(self):
        """The expression string"""
        return self._text
    
    @text.setter
    def text(self, value):
        self._parse_needed = True
        self._text = value

    @property
    def scope(self):
        """The scoping object used to evaluate the expression"""
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
        
    def _parse(self):
        self._allow_set = True
        self.var_names = set()
        new_ast = ExprTransformer(self).visit(self._pre_parse())
        
        # compile the transformed AST
        ast.fix_missing_locations(new_ast)
        mode = 'exec' if isinstance(new_ast, ast.Module) else 'eval'
        self._code = compile(new_ast, '<string>', mode)
        
        if self._allow_set: # set up a compiled assignment statement
            assign_txt = "%s=_local_setter" % self.text
            root = ast.parse(assign_txt, mode='exec')
            ## transform into a 'set' call to set the specified variable
            assign_ast = ExprTransformer(self).visit(root)
            ast.fix_missing_locations(assign_ast)
            self._assignment_code = compile(assign_ast,'<string>','exec')
            
        self._parse_needed = False
        return new_ast
                
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
        
    def get_metadata(self, metaname=None, scope=None):
        """Return the specified piece of metadata if metaname is provided. Otherwise
        return the whole metadata dictionary.  
        
        Returns a list of tuples containing (varname, metadata) 
        corresponding to each variable referenced by this expression.
        """
        varnames = self.get_referenced_varpaths()
        scope = self._get_updated_scope(scope)
        lst = []
        for name in varnames:
            if scope.contains(name):
                lst.append((name, scope.get_metadata(name, metaname)))
            #elif scope.parent and scope.parent.contains(name):
                #lst.append((name, scope.parent.get_metadata(name, metaname)))
            else:
                raise AttributeError("'%s' not found" % name)
        return lst

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
        scope = self.scope
        if scope: # and scope.parent:
            if self._parse_needed:
                self._parse()
            #if not all(scope.parent.get_valid(self.var_names)):
            if not all(scope.get_valid(self.var_names)):
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
                #if scope.parent:
                    #scope = scope.parent
                for name in self.var_names:
                    if not scope.contains(name):
                        return False
                return True
            return False
        return True
    
    def __eq__(self,other):
        if isinstance(other,self.__class__): 
            return self.text == other.text
        return False

    def __repr__(self): 
        return '<ExprEval(text=%s)>' % self._text
    
    def __str__(self):
        return self._text

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
    
    
