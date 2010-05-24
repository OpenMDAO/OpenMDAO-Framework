

class Link(object):
    def __init__(self, scope, source, sink, expr=None, mapping=None):
        """
        scope is the enclosing Assembly
        source is component supplying outputs
        sink is the component supplying inputs
        expr is an optional boolean expression string. If present, the link
                is active only if the expression evaluates to True
        mapping is a dict that maps outputs to the inputs that they connect to.
                The dict has the form:
                { <outvarname1>: <invarname1>,
                  <outvarname2>: (<invarname2>,<invarname3>)  # if output connects to more than 1 input
                }
                where the <varname>s do not include the name of their parent component.  For example:
                { 'a': 'b',
                  'c': ('d','e')
                }
        """
        self.scope = scope
        self.source = source
        self.sink = sink
        if isinstance(expr, basestring):
            self.expr = Expression(expr, scope=scope)
        else:
            self.expr = self.expr
        if mapping:
            self.mapping = mapping.copy()
        else:
            self.mapping = {}
        
    