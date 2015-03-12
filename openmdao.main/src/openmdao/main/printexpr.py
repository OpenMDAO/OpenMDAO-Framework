
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
        self.append = self.txtlist.append

    def get_text(self):
        return ''.join(self.txtlist)

    def visit_Attribute(self, node):
        self.visit(node.value)
        self.append(".%s" % node.attr)

    def visit_Assign(self, node):
        for i,t in enumerate(node.targets):
            if i>0: self.append(',')
            self.visit(t)
        self.append(' = ')
        self.visit(node.value)

    def visit_Name(self, node):
        self.append(node.id)

    def visit_UnaryOp(self, node):
        if isinstance(node.operand, ast.BinOp):
            self.visit(node.op)
            self.append('(')
            self.visit(node.operand)
            self.append(')')
        else:
            super(ExprPrinter, self).generic_visit(node)

    def visit_BinOp(self, node):
        # we have to add parens around any immediate BinOp child
        # that has a lower precedence operation than we do
        if isinstance(node.left, ast.BinOp) and _pred_cmp(node.left.op, node.op) < 0:
            self.append('(')
            self.visit(node.left)
            self.append(')')
        else:
            self.visit(node.left)
        self.visit(node.op)
        if isinstance(node.right, ast.BinOp):
            pred_comp = _pred_cmp(node.right.op, node.op)
            # Subtraction isn't commutative, so when the operator precedence
            # is equal, we still need parentheses.
            if pred_comp < 0 or \
              (pred_comp == 0 and (isinstance(node.op, ast.Sub) or isinstance(node.op, ast.Div))):
                self.append('(')
                self.visit(node.right)
                self.append(')')
            else:
                self.visit(node.right)
        else:
            self.visit(node.right)

    def visit_IfExp(self, node):
        self.visit(node.body)
        self.append(' if ')
        self.visit(node.test)
        self.append(' else ')
        self.visit(node.orelse)

    def visit_Call(self, node):
        self.visit(node.func)
        self.append('(')
        total_args = 0
        for arg in node.args:
            if total_args>0: self.append(',')
            self.visit(arg)
            total_args += 1

        if hasattr(node, 'keywords'):
            for kw in node.keywords:
                if total_args>0: self.append(',')
                self.visit(kw)
                total_args += 1

        if hasattr(node, 'starargs'):
            if node.starargs:
                if total_args>0: self.append(',')
                self.append('*%s'%node.starargs)
                total_args += 1

        if hasattr(node, 'kwargs'):
            if node.kwargs:
                if total_args>0: self.append(',')
                self.append('**%s'%node.kwargs)

        self.append(')')

    def visit_keyword(self, node):
        self.append("%s=" % node.arg)
        self.visit(node.value)

    def visit_Num(self, node):
        self.append(str(node.n))

    def visit_Str(self, node):
        self.append("'%s'" % node.s)

    def visit_Subscript(self, node):
        self.visit(node.value)
        self.append('[')
        self.visit(node.slice)
        self.append(']')

    def visit_Index(self, node):
        self.visit(node.value)

    def visit_Slice(self, node):
        if node.lower is not None:
            if not(isinstance(node.lower, ast.Name) and node.lower.id == 'None'):
                self.visit(node.lower)
        self.append(':')
        if node.upper is not None:
            if not(isinstance(node.upper, ast.Name) and node.upper.id == 'None'):
                self.visit(node.upper)
        self.append(':')
        if node.step is not None:
            if not(isinstance(node.step, ast.Name) and node.step.id == 'None'):
                self.visit(node.step)

    def visit_ExtSlice(self, node):
        for i,d in enumerate(node.dims):
            if i>0:
                self.append(',')
            self.visit(d)

    def visit_List(self, node):
        self.append('[')
        for i,e in enumerate(node.elts):
            if i>0: self.append(',')
            self.visit(e)
        self.append(']')

    def visit_Dict(self, node):
        self.append('{')
        for i,tup in enumerate(zip(node.keys,node.values)):
            if i>0: self.append(',')
            self.append("'%s':" % tup[0].s)
            self.visit(tup[1])
        self.append('}')

    def visit_Tuple(self, node):
        self.append('(')
        length = len(node.elts)
        for i,e in enumerate(node.elts):
            if i>0: self.append(',')
            self.visit(e)
        if length==1: self.append(',')
        self.append(')')

    def visit_USub(self, node): self.append('-')
    def visit_UAdd(self, node): self.append('+')
    def visit_And(self, node):  self.append(' and ')
    def visit_Or(self, node):   self.append(' or ')

    # operators
    def visit_Add(self, node):      self.append('+')
    def visit_Sub(self, node):      self.append('-')
    def visit_Mult(self, node):     self.append('*')
    def visit_Div(self, node):      self.append('/')
    def visit_Mod(self, node):      self.append('%')
    def visit_Pow(self, node):      self.append('**')
    def visit_LShift(self, node):   self.append('<<')
    def visit_Rshift(self, node):   self.append('>>')
    def visit_BitOr(self, node):    self.append('|')
    def visit_BitXor(self, node):   self.append('^')
    def visit_BitAnd(self, node):   self.append('&')
    def visit_FloorDiv(self, node): self.append('//')

    # cmp operators
    def visit_Eq(self, node):    self.append('==')
    def visit_NotEq(self, node): self.append('!=')
    def visit_Lt(self, node):    self.append('<')
    def visit_LtE(self, node):   self.append('<=')
    def visit_Gt(self, node):    self.append('>')
    def visit_GtE(self, node):   self.append('>=')
    def visit_Is(self, node):    self.append(' is ')
    def visit_IsNot(self, node): self.append(' is not ')
    def visit_In(self, node):    self.append(' in ')
    def visit_NotIn(self, node): self.append(' not in ')

    def _ignore(self, node):
        super(ExprPrinter, self).generic_visit(node)

    visit_Module     = _ignore
    visit_Expr       = _ignore
    visit_Expression = _ignore
    visit_Compare    = _ignore
    #visit_UnaryOp    = _ignore
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

    def visit_Subscript(self, node):
        p = print_node(node)
        xform = self.mapping.get(p)
        if xform is not None:
            return _get_attr_node(xform.split('.'))
        return super(ExprNameTransformer, self).generic_visit(node)


def transform_expression(expr, mapping):
    """Returns a new expression string with the names transformed based on
    the value of the mapping dict.  Note that this transforms only from
    the beginning of a name, so for example, if you have abc.xyz.abc and
    a mapping of { 'abc': 'XXX' }, you'll get 'XXX.xyz.abc', not
    'XXX.xyz.XXX'.
    """
    if expr in mapping:
        return mapping[expr]

    new_ast = ExprNameTransformer(mapping).visit(ast.parse(expr, mode='eval'))
    ast.fix_missing_locations(new_ast)

    ep = ExprPrinter()
    ep.visit(new_ast)
    return ep.get_text()

def eliminate_expr_ws(expr):
    """Return the expression string with whitespace removed, except for
    whitespace within string literals passed as function args.
    """
    node = ast.parse(expr, mode='eval')
    ep = ExprPrinter()
    ep.visit(node)
    return ep.get_text()

def print_node(node):
    p = ExprPrinter()
    p.visit(node)
    return p.get_text()


if __name__ == '__main__':
    import sys
    mapping = { 'foo.bar': 'a.b.c.de', 'blah': 'hohum' }
    print transform_expression(sys.argv[1], mapping)
