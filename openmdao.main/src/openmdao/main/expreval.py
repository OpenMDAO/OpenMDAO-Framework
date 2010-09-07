# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]


import weakref
import math

from pyparsing import Word, ZeroOrMore, OneOrMore, Literal, CaselessLiteral
from pyparsing import oneOf, alphas, nums, alphanums, Optional, Combine
from pyparsing import Forward, StringEnd
from pyparsing import ParseException

_evalglobals = {
    'abs': abs, 
    'all': all, 
    'any': any, 
    'bool': bool, 
    'complex': complex, 
    'divmod': divmod, 
    'filter': filter, 
    'float': float, 
    'hex': hex, 
    'int': int, 
    'isinstance': isinstance, 
    'issubclass': issubclass, 
    'iter': iter, 
    'len': len, 
    'list': list, 
    'long': long, 
    'math': math,
    'max': max, 
    'min': min, 
    'oct': oct, 
    'ord': ord, 
    'pow': pow, 
    'range': range, 
    'reduce': reduce, 
    'reversed': reversed, 
    'round': round, 
    'set': set, 
    'sorted': sorted, 
    'str': str, 
    'sum': sum, 
    'tuple': tuple
}

_fake_eq = '_@EQ@_'  # used to replace '==' to avoid ambiguity with '='

def _cannot_find(name):
    raise RuntimeError("ExprEvaluator: cannot find variable '%s'" % name)

def _trans_unary(strng, loc, tok):
    return tok

    
def _trans_lhs(strng, loc, tok, exprobj):
    if exprobj._scope:
        scope = exprobj._scope()
        lazy_check = exprobj.lazy_check
        if scope.contains(tok[0]):
            scname = 'scope'
        else:
            scname = 'scope.parent'
            if lazy_check is False and not scope.parent.contains(tok[0]):
                _cannot_find(tok[0])
        exprobj.var_names.add(tok[0])
    else:
        _cannot_find(tok[0])
        
    full = scname + ".set('" + tok[0] + "',_@RHS@_"
    if len(tok) > 1 and tok[1] != '=':
        full += ","+tok[1]
            
    return ['=', full + ")"]
    
def _trans_assign(strng, loc, tok, exprobj):
    if tok[0] == '=':
        return [tok[1].replace('_@RHS@_', tok[2], 1)]
    else:
        return tok
    
def _trans_arrayindex(strng, loc, tok):
    full = "[" + tok[1]
    if tok[2] == ',':
        for index in range(3, len(tok), 2):
            full += ','
            full += tok[index]
    else:
        for index in range(4, len(tok), 3):
            full += ','
            full += tok[index]
    return [full+"]"]
    
def _trans_arglist(strng, loc, tok):
    full = "("
    if len(tok) > 2: 
        full += tok[1]
    for index in range(3, len(tok), 2):
        full += ','+tok[index]
    return [full+")"]

def _trans_fancyname(strng, loc, tok, exprobj):
    # if we find the named object in the current scope, then we don't need to 
    # do any translation.  The scope object is assumed to have a contains() 
    # function.
    if exprobj._scope is None:
        _cannot_find(tok[0])
    
    scope = exprobj._scope()
    lazy_check = exprobj.lazy_check
    
    if tok[0].startswith('math.'):
        exprobj._has_globals = True
        return tok
    elif scope.contains(tok[0]):
        scname = 'scope'
        if hasattr(scope, tok[0]):
            return tok  # use name unmodified for faster local access
    elif tok[0] == '_local_setter': # used in assigment statements
        return tok
    else:
        scname = 'scope.parent'
        if lazy_check is False and (scope.parent is None or 
                                 not scope.parent.contains(tok[0])):
            _cannot_find(tok[0])
    
        
    if len(tok) == 1 or (len(tok) > 1 and tok[1].startswith('[')):
        full = scname + ".get('" + tok[0] + "'"
        if len(tok) > 1:
            full += ","+tok[1]
        exprobj.var_names.add(tok[0])
        exprobj._comp_names[tok[0].split('.')[0]] = 0
    else:
        # special check here for calls to builtins like abs, all, any, etc.
        # or calls to math functions (math.sin, math.cos, etc.)
        if tok[0] in _evalglobals or tok[0].startswith('math.'):
            exprobj._has_globals = True
            full = tok[0] + "("
            if len(tok[1]) > 2:
                full += tok[1][1:-1]
        else:
            full = scname + ".invoke('" + tok[0] + "'"
            if len(tok[1]) > 2:
                full += "," + tok[1][1:-1]
        
    return [full + ")"]
    

def translate_expr(text, exprobj, single_name=False):
    """A function to translate an expression using dotted names into a new
    expression string containing the appropriate calls to resolve those dotted
    names in the framework, e.g., 'a.b.c' becomes get('a.b.c') or 'a.b.c(1,2,3)'
    becomes invoke('a.b.c',1,2,3).
    """
    lazy_check = exprobj.lazy_check
    
    ee = CaselessLiteral('E')
    comma    = Literal( "," )    
    plus     = Literal( "+" )
    minus    = Literal( "-" )
    mult     = Literal( "*" )
    div      = Literal( "/" )
    lpar     = Literal( "(" )
    rpar     = Literal( ")" )
    dot      = Literal( "." )
    
    equal    = Literal( _fake_eq ) # substitute for '==' to avoid ambiguity in grammar
    notequal = Literal( "!=" )
    less     = Literal( "<" )
    lesseq   = Literal( "<=" )
    greater  = Literal( ">" )
    greatereq = Literal( ">=" )
    
    assignop = Literal( "=" )
    lbracket = Literal( "[" )
    rbracket = Literal( "]" )
    expop    = Literal( "**" )

    expr = Forward()
    arrayindex = Forward()
    arglist = Forward()
    fancyname = Forward()

    digits = Word(nums)

    number = Combine( ((digits + Optional( dot + Optional(digits) )) |
                             (dot + digits) )+
                           Optional( ee + Optional(oneOf('+ -')) + digits )
                          )
    name = Word('_'+alphas, bodyChars='_'+alphanums)
    pathname = Combine(name + ZeroOrMore(dot + name))
    arrayindex << OneOrMore(lbracket + Combine(expr) + 
                            ZeroOrMore(comma+Combine(expr)) + rbracket)
    arrayindex.setParseAction(_trans_arrayindex)
    arglist << lpar + Optional(Combine(expr) + 
                               ZeroOrMore(comma+Combine(expr))) + rpar
    arglist.setParseAction(_trans_arglist)
    fancyname << pathname + ZeroOrMore(arrayindex | arglist)
    
    # set up the scope name translation here. Parse actions called from
    # pyparsing only take 3 args, so we wrap our function in a lambda function
    # with an extra argument to specify the ExprEvaluator object.
    fancyname.setParseAction(
        lambda s,loc,tok: _trans_fancyname(s,loc,tok,exprobj))

    addop  = plus | minus
    multop = mult | div
    boolop = equal | notequal | lesseq | less | greatereq | greater 

    factor = Forward()
    atom = Combine(Optional("-") + (( number | fancyname) | (lpar+expr+rpar)), adjacent=False)
    factor << atom + ZeroOrMore( ( expop + factor ) )
    term = factor + ZeroOrMore( ( multop + factor ) )
    addterm = term + ZeroOrMore( ( addop + term ) )
    expr << addterm + ZeroOrMore( ( boolop + addterm ) )
    
    lhs_fancyname = pathname + ZeroOrMore(arrayindex)
    lhs = lhs_fancyname + assignop
    lhs.setParseAction(lambda s,loc,tok: _trans_lhs(s,loc,tok,exprobj))
    equation = Optional(lhs) + Combine(expr) + StringEnd()
    equation.setParseAction(lambda s,loc,tok: _trans_assign(s,loc,tok, exprobj))
    
    try:
        if single_name:
            simple_str = fancyname + StringEnd()
            return ''.join(simple_str.parseString(text))
        else:
            return ''.join(equation.parseString(text))
    except ParseException, err:
        raise RuntimeError(str(err)+' - '+err.markInputline())


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
        if scope is None:
            s._scope = None
        else:
            s._scope = weakref.ref(scope)
        s.lazy_check = lazy_check
        s.single_name = single_name
        s._text = None  # used to detect change in str
        s.var_names = set()
        s._comp_names = {}
        s.scoped_text = None
        s._has_globals = False
        if lazy_check is False:
            s._parse()
        return s
        
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
        self._text = str(self).replace('==', _fake_eq)
        self.var_names = set()
        self._comp_names = {}
        self.scoped_text = translate_expr(self._text, self, 
                                          single_name=self.single_name).replace(_fake_eq,'==')
        self._code = compile(self.scoped_text, '<string>','eval')
        if self.single_name: # set up a compiled assignment statement
            old_lazy_check = self.lazy_check
            try:
                self.lazy_check = True
                self.scoped_assignment_text = translate_expr(
                                            '%s = _local_setter' % self._text, 
                                            self).replace(_fake_eq,'==')
                self._assignment_code = compile(self.scoped_assignment_text, 
                                                '<string>','exec')
            finally:
                self.lazy_check = old_lazy_check
        self._text = self._text.replace(_fake_eq, '==')
                
    def evaluate(self):
        """Return the value of the scoped string, evaluated 
        using the eval() function.
        """
        if self._scope is not None:
            scope = self._scope()
            
            if scope is None:
                raise RuntimeError(
                        'ExprEvaluator scoping object does not exist.')
        else:
            scope = None
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
        if self._scope:
            scope = self._scope()
        else:
            scope = None
        # object referred to by weakref may no longer exist
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
                if not all(scope.parent.get_valids(self.var_names)):
                    return False
        return True
    
