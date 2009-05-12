
# pylint: disable-msg=W0104,R0914

#public symbols
__all__ = ["ExprEvaluator"]

__version__ = "0.1"


#from __future__ import division

import weakref

from pyparsing import Word, ZeroOrMore, OneOrMore, Literal, CaselessLiteral
from pyparsing import oneOf, alphas, nums, alphanums, Optional, Combine
from pyparsing import Forward, StringEnd
from pyparsing import ParseException

def _trans_unary(strng, loc, tok):
    return tok

    
def _trans_lhs(strng, loc, tok, scope, validate):
    if scope.contains(tok[0]):
        scname = 'scope'
    else:
        scname = 'scope.parent'
        if hasattr(scope, tok[0]):
            scope.warning("attribute '"+tok[0]+"' is private"+
                          " so a public value in the parent is"+
                          " being used instead (if found)")
        if validate is True and not scope.parent.contains(tok[0]):
            raise RuntimeError("cannot find variable '"+tok[0]+"'")
    
    full = scname + ".set('" + tok[0] + "',_@RHS@_"
    if len(tok) > 1 and tok[1] != '=':
        full += ","+tok[1]
            
    return ['=', full + ")"]
    
def _trans_assign(strng, loc, tok, exprobj):
    if tok[0] == '=':
        exprobj.lhs = tok[1].replace('_@RHS@_', '', 1)
        exprobj.rhs = tok[2]
        return [tok[1].replace('_@RHS@_', tok[2], 1)]
    else:
        exprobj.lhs = ''.join(tok)
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

def _trans_fancyname(strng, loc, tok, scope, validate):
    # if we find the named object in the current scope, then we don't need to 
    # do any translation.  The scope object is assumed to have a contains() 
    # function.
    if scope.contains(tok[0]):
        scname = 'scope'
        if hasattr(scope, tok[0]):
            return tok  # use name unmodified for faster local access
    else:
        scname = 'scope.parent'
        if hasattr(scope, tok[0]):
            scope.warning("attribute '"+tok[0]+"' is private"+
                          " so a public value in the parent is"+
                          " being used instead (if found)")
        if validate is True and (scope.parent is None or 
                                 not scope.parent.contains(tok[0])):
            raise RuntimeError("cannot find variable '"+tok[0]+"'")
    
    if len(tok) == 1 or (len(tok) > 1 and tok[1].startswith('[')):
        full = scname + ".get('" + tok[0] + "'"
        if len(tok) > 1:
            full += ","+tok[1]
    else:
        full = scname + ".invoke('" + tok[0] + "'"
        if len(tok[1]) > 2:
            full += "," + tok[1][1:-1]
        
    return [full + ")"]
    

def translate_expr(text, exprobj):
    """A function to translate an expression using dotted names into a new
    expression string containing the appropriate calls to resolve those dotted
    names in the framework, e.g., get('a.b.c') or invoke('a.b.c',1,2,3).
    """
    scope = exprobj._scope()
    validate = exprobj.validate
    
    ee = CaselessLiteral('E')
    comma    = Literal( "," )    
    plus     = Literal( "+" )
    minus    = Literal( "-" )
    mult     = Literal( "*" )
    div      = Literal( "/" )
    lpar     = Literal( "(" )
    rpar     = Literal( ")" )
    dot      = Literal( "." )
#    equal    = Literal( "==" )
#    notequal = Literal( "!=" )
#    less     = Literal( "<" )
#    lesseq   = Literal( "<=" )
#    greater  = Literal( ">" )
#    greatereq = Literal( ">=" )
    
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
    # with an extra argument to specify the scope used for the translation.
    fancyname.setParseAction(
        lambda s,loc,tok: _trans_fancyname(s,loc,tok,scope, validate))

    addop  = plus | minus
    multop = mult | div
#    boolop = equal | notequal | less | lesseq | greater | greatereq

    factor = Forward()
    atom = Combine(Optional("-") + (( number | fancyname) | (lpar+expr+rpar)))
    factor << atom + ZeroOrMore( ( expop + factor ) )
    term = factor + ZeroOrMore( ( multop + factor ) )
    expr << term + ZeroOrMore( ( addop + term ) )
    
    lhs_fancyname = pathname + ZeroOrMore(arrayindex)
    lhs = lhs_fancyname + assignop
    lhs.setParseAction(lambda s,loc,tok: _trans_lhs(s,loc,tok,scope,validate))
    equation = Optional(lhs) + Combine(expr) + StringEnd()
    equation.setParseAction(lambda s,loc,tok: _trans_assign(s,loc,tok, exprobj))
    
    try:
        return ''.join(equation.parseString(text))
    except ParseException, err:
        raise RuntimeError(str(err)+' - '+err.markInputline())

    
class ExprEvaluator(object):
    """A class that translates an expression string into a new string containing
    any necessary framework access functions, e.g., set, get. The compiled
    bytecode is stored within the object so that it doesn't have to be reparsed
    during later evaluations.  A scoping object is required at construction time
    and that object determines the form of the  translated expression. 
    Variables that are local to the scoping object do not need to be translated,
    whereas variables from other objects must  be accessed using the appropriate
    set() or get() call.  Array entry access and function invocation are also
    translated in a similar way.  For example, the expression "a+b[2]-comp.y(x)"
    for a scoping object that contains attributes a and b, but not comp,x or y,
    would translate to 
    "a+b[2]-self.parent.invoke('comp.y',self.parent.get('x'))".
    
    If validate is True, any objects referenced in the expression must exist
    at creation time (or any time later that text is set to a different value)
    or a RuntimeError will be raised.
    """
    
    def __init__(self, text, scope, validate=True):
        self._scope = weakref.ref(scope)
        self.validate = validate
        self.text = text
        self.rhs = ''
        self.lhs = ''
    
    def __getstate__(self):
        """Return dict representing this container's state."""
        state = self.__dict__.copy()
        if state['_scope'] is not None:
            # remove weakref to scope because it won't pickle
            state['_scope'] = self._scope()
        state['_code'] = None  # <type 'code'> won't pickle either.
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        self.__dict__ = state
        if self._scope is not None:
            self._scope = weakref.ref(self._scope)
        if self.scoped_text:
            self._code = compile(self.scoped_text, '<string>', 'eval')

    def _set_text(self, text):
        self._text = text
        self.scoped_text = translate_expr(text, self)
        self._code = compile(self.scoped_text, '<string>', 'eval')
        
    def _get_text(self):
        return self._text
    
    text = property(_get_text, _set_text, None,
                    'The original text of the expression.')
        
    def evaluate(self, scope=None):
        """Return the value of the scoped string, evaluated 
        using the eval() function.
        """
        if scope is None:
            scope = self._scope()
        # object referred to by weakref may no longer exist
        if scope is None:
            raise RuntimeError(
                'ExprEvaluator cannot evaluate expression without scope.')
        
        return eval(self._code, scope.__dict__, locals())

