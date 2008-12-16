"""
A translator that converts dotted names to the appropriate framework calls. If the dotted
named object is not found in the given scope, it will search the parent scope, but no higher.
For example, for a reference like a.b[2+x], if a.b is not found in the current scope but x is, 
the result would be self.parent.get('a.b',[2+x]).
"""

#public symbols
__all__ = ["ExprEvaluator"]

__version__ = "0.1"


#from __future__ import division

import weakref

from pyparsing import Word,Group,ZeroOrMore,OneOrMore,Literal,CaselessLiteral,Each
from pyparsing import oneOf,alphas,nums,alphanums,quotedString,Optional,Combine,Forward,StringEnd
from pyparsing import ParseException,traceParseAction

def _trans_unary(s, loc, tok):
    return tok

    
def _trans_lhs(s, loc, tok, scope):
    if scope.contains(tok[0]):
        scname = 'scope'
    else:
        scname = 'scope.parent'
        if hasattr(scope,tok[0]):
            scope.warning("attribute '"+tok[0]+"' is private"+
                          " so a public value in the parent is"+
                          " being used instead (if found)")
    
    full = scname + ".set('" + tok[0] + "',_@RHS@_"
    if len(tok) > 1:
        full += ","+tok[1]
            
    return ['=',full + ")"]
    
def _trans_assign(s, loc, tok, scope):
    if tok[0] == '=':
        return [tok[1].replace('_@RHS@_',tok[2],1)]
    else:
        return tok
    
def _trans_arrayindex(s, loc, tok):
    full = "[" + tok[1]
    if tok[2] == ',':
        for index in range(3,len(tok),2):
            full += ','
            full += tok[index]
    else:
        for index in range(4,len(tok),3):
            full += ','
            full += tok[index]
    return [full+"]"]
    
def _trans_arglist(s, loc, tok):
    full = "("
    if len(tok) > 2: full += tok[1]
    for index in range(3,len(tok),2):
        full += ','+tok[index]
    return [full+")"]

def _trans_fancyname(s, loc, tok, scope):
    # if we find the named object in the current scope, then we don't need to 
    # do any translation.  The scope object is assumed to have a contains() function.
    if scope.contains(tok[0]):
        scname = 'scope'
        if hasattr(scope,tok[0]):
            return tok  # use name unmodified for faster local access
    else:
        scname = 'scope.parent'
        if hasattr(scope,tok[0]):
            scope.warning("attribute '"+tok[0]+"' is private"+
                          " so a public value in the parent is"+
                          " being used instead (if found)")
    
    if len(tok) == 1 or (len(tok) > 1 and tok[1].startswith('[')):
        full = scname + ".get('" + tok[0] + "'"
        if len(tok) > 1:
            full += ","+tok[1]
    else:
        full = scname + ".invoke('" + tok[0] + "'"
        if len(tok[1]) > 2:
            full += "," + tok[1][1:-1]
        
    return [full + ")"]
    

def translate_expr(text, scope):
    """A function to translate an expression using dotted names into a new expression string
    containing the appropriate calls to resolve those dotted names in the framework, e.g.,
    get('a.b.c') or invoke('a.b.c',1,2,3).
    """
    
    e = CaselessLiteral('E')
    comma    = Literal( "," )    
    plus     = Literal( "+" )
    minus    = Literal( "-" )
    mult     = Literal( "*" )
    div      = Literal( "/" )
    lpar     = Literal( "(" )
    rpar     = Literal( ")" )
    dot      = Literal( "." )
    equal    = Literal( "==" )
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
                           Optional( e + Optional(oneOf('+ -')) + digits )
                          )
    name = Word('_'+alphas, bodyChars='_'+alphanums)
    pathname = Combine(name + ZeroOrMore(dot + name))
    arrayindex << OneOrMore(lbracket + Combine(expr) + ZeroOrMore(comma+Combine(expr)) + rbracket)
    arrayindex.setParseAction(_trans_arrayindex)
    arglist << lpar + Optional(Combine(expr) + ZeroOrMore(comma+Combine(expr))) + rpar
    arglist.setParseAction(_trans_arglist)
    fancyname << pathname + ZeroOrMore(arrayindex | arglist)
    
    # set up the scope name translation here.  Parse actions called from pyparsing
    # only take 3 args, so we wrap our function in a lambda function with an extra
    # argument to specify the scope used for the translation.
    fancyname.setParseAction(lambda s,loc,tok: _trans_fancyname(s,loc,tok,scope))

    addop  = plus | minus
    multop = mult | div
    boolop = equal | notequal | less | lesseq | greater | greatereq

    factor = Forward()
    atom = Combine(Optional("-") + (( number | fancyname) | (lpar + expr + rpar)))
    factor << atom + ZeroOrMore( ( expop + factor ) )
    term = factor + ZeroOrMore( ( multop + factor ) )
    expr << term + ZeroOrMore( ( addop + term ) )
    
    lhs_fancyname = pathname + ZeroOrMore(arrayindex)
    lhs = lhs_fancyname + assignop
    lhs.setParseAction(lambda s,loc,tok: _trans_lhs(s,loc,tok,scope))
    equation = Optional(lhs) + Combine(expr) + StringEnd()
    equation.setParseAction(lambda s,loc,tok: _trans_assign(s,loc,tok,scope))
    
    try:
        return ''.join(equation.parseString(text))
    except ParseException, err:
        raise RuntimeError(str(err)+' - '+err.markInputline())

    
class ExprEvaluator(object):
    def __init__(self, text, scope):
        self.scope = weakref.ref(scope)
        self.set_text(text)
    
    def set_text(self, text):
        self.text = text
        self.scoped_text = translate_expr(text, self.scope())
        self._code = compile(text,'<string>','eval')
        
    def evaluate(self, scope=None):
        if scope is None:
            scope = self.scope()
        if scope is None:
            raise RuntimeError('ExprEvaluator cannot evaluate expression without scope.')
        
        return eval(self._code, self.scope().__dict__, locals())

    
    
