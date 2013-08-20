
import ast



def _get_attr_node(names):
    """Builds an Attribute node, or a Name node if names has just one entry."""
    node = ast.Name(id=names[0], ctx=ast.Load())
    for name in names[1:]:
        node = ast.Attribute(value=node, attr=name, ctx=ast.Load())
    return node

def _get_long_name(node):
    # If the node is an Attribute or Name node that is composed
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
    """Used to determine operator precedence."""
    return _op_preds[op1.__class__] - _op_preds[op2.__class__]

class ExprPrinter(ast.NodeVisitor):
    """A NodeVisitor that gets the Python text of an expression or assignment
    statement defined by an AST.
    """
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
        
    def visit_keyword(self, node):
        self.write("%s=" % node.arg)
        self.visit(node.value)
        
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
            if not(isinstance(node.lower, ast.Name) and node.lower.id == 'None'):
                self.visit(node.lower)
        self.write(':')
        if node.upper is not None:
            if not(isinstance(node.upper, ast.Name) and node.upper.id == 'None'):
                self.visit(node.upper)
        self.write(':')
        if node.step is not None:
            if not(isinstance(node.step, ast.Name) and node.step.id == 'None'):
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

    visit_Module     = _ignore
    visit_Expr       = _ignore
    visit_Expression = _ignore
    visit_Compare    = _ignore
    visit_UnaryOp    = _ignore
    visit_Subscript  = _ignore
    visit_Load       = _ignore
    visit_Store      = _ignore
    
    def generic_visit(self, node):
        # We want to fail if we see any nodes we don't know about rather than
        # generating code that isn't correct.
        raise RuntimeError("ExprPrinter can't handle a node of type %s" % node.__class__.__name__)

    

class ExprNameTransformer(ast.NodeTransformer):
    def __init__(self, mapping):
        self.mapping = mapping.copy()
        super(ExprNameTransformer, self).__init__()
        
    def visit_Name(self, node):
        return ast.Name(id=self.mapping.get(node.id, node.id), 
                        ctx=ast.Load())
    
    def visit_Attribute(self, node):
        long_name = _get_long_name(node)
        if long_name is None or long_name not in self.mapping:
            return self.generic_visit(node)
        return _get_attr_node(self.mapping[long_name].split('.'))

    
def transform_expression(expr, mapping):
    """Returns a new expression string with the names transformed based on
    the value of the mapping dict.  Note that this transforms only "complete"
    names (dotted or not), not sub-names within a larger dotted name.
    """
    new_ast = ExprNameTransformer(mapping).visit(ast.parse(expr, mode='eval'))
    ast.fix_missing_locations(new_ast)
    
    ep = ExprPrinter()
    ep.visit(new_ast)
    return ep.get_text()


def eliminate_expr_ws(expr):
    """Return the expression string with whitespace removed, except for 
    whitespace within string literals passed as function args.
    """
    return transform_expression(expr, {})

if __name__ == '__main__':
    import sys
    mapping = { 'foo.bar': 'a.b.c.def', 'blah': 'hohum' }
    print transform_expression(sys.argv[1], mapping)
    
    
    
