
import networkx as nx
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.pseudocomp import PseudoComponent


class ExprMapper(object):
    """A mapping between source expressions and destination expressions"""
    def __init__(self, scope):
        self._exprgraph = nx.DiGraph()  # graph of source expressions to destination expressions
        self._scope = scope
        self._pseudo_count = 0

    def get_output_exprs(self):
        """Return all destination expressions at the output boundary"""
        exprs = []
        graph = self._exprgraph
        for node, data in graph.nodes(data=True):
            if graph.in_degree(node) > 0:
                expr = data['expr']
                if len(expr.get_referenced_compnames()) == 0:
                    exprs.append(expr)
        return exprs

    def get_expr(self, text):
        node = self._exprgraph.node.get(text)
        if node:
            return node['expr']
        return None

    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        excludes = set([name for name, data in self._exprgraph.nodes(data=True)
                        if data['expr'].refs_parent()])
        if show_passthrough:
            return [(u, v) for u, v in self._exprgraph.edges() if not (u in excludes or v in excludes)]
        else:
            return [(u, v) for u, v in self._exprgraph.edges()
                       if '.' in u and '.' in v and not (u in excludes or v in excludes)]

    def get_source(self, dest_expr):
        """Returns the text of the source expression that is connected to the given
        destination expression.
        """
        dct = self._exprgraph.pred.get(dest_expr)
        if dct:
            return dct.keys()[0]
        else:
            return None

    def get_dests(self, src_expr):
        """Returns the list of destination expressions that are connected to the given
        source expression.
        """
        graph = self._exprgraph
        return [graph.node(name)['expr'] for name in self._exprgraph.succ[src_expr].keys()]

    def remove(self, compname):
        """Remove any connections referring to the given component"""
        refs = self.find_referring_exprs(compname)
        if refs:
            self._exprgraph.remove_nodes_from(refs)
            self._remove_disconnected_exprs()

    def connect(self, srcexpr, destexpr, scope, pseudocomp=None):
        src = srcexpr.text
        dest = destexpr.text
        srcvars = srcexpr.get_referenced_varpaths(copy=False)
        destvar = destexpr.get_referenced_varpaths().pop()

        destcompname, destcomp, destvarname = scope._split_varpath(destvar)
        desttrait = None
        srccomp = None

        if not isinstance(destcomp, PseudoComponent) and not destvar.startswith('parent.') and not len(srcvars)>1:
            for srcvar in srcvars:
                if not srcvar.startswith('parent.'):
                    srccompname, srccomp, srcvarname = scope._split_varpath(srcvar)
                    if not isinstance(srccomp, PseudoComponent):
                        src_io = 'in' if srccomp is scope else 'out'
                        srctrait = srccomp.get_dyn_trait(srcvarname, src_io)
                        if desttrait is None:
                            dest_io = 'out' if destcomp is scope else 'in'
                            desttrait = destcomp.get_dyn_trait(destvarname, dest_io)

                if not isinstance(srccomp, PseudoComponent) and not srcexpr.refs_parent() and desttrait is not None:
                    # punt if dest is not just a simple var name.
                    # validity will still be checked at execution time
                    if destvar == destexpr.text:
                        ttype = desttrait.trait_type
                        if not ttype:
                            ttype = desttrait
                        srcval = srcexpr.evaluate()
                        if ttype.validate:
                            ttype.validate(destcomp, destvarname, srcval)
                        else:
                            # no validate function on destination trait. Most likely
                            # it's a property trait.  No way to validate without
                            # unknown side effects. Have to wait until later when
                            # data actually gets passed via the connection.
                            pass

        if src not in self._exprgraph:
            self._exprgraph.add_node(src, expr=srcexpr)
        if dest not in self._exprgraph:
            self._exprgraph.add_node(dest, expr=destexpr)

        self._exprgraph.add_edge(src, dest)
        if pseudocomp is not None:
            self._exprgraph[src][dest]['pcomp'] = pseudocomp

    def find_referring_exprs(self, name):
        """Returns a list of expression strings that reference the given name, which
        can refer to either a variable or a component.
        """
        return [node for node, data in self._exprgraph.nodes(data=True) 
                       if data['expr'].refers_to(name)]

    def _remove_disconnected_exprs(self):
        # remove all expressions that are no longer connected to anything
        to_remove = []
        graph = self._exprgraph
        for expr in graph.nodes():
            if graph.in_degree(expr) == 0 and graph.out_degree(expr) == 0:
                to_remove.append(expr)
        graph.remove_nodes_from(to_remove)
        return to_remove

    def disconnect(self, srcpath, destpath=None):
        """Disconnect the given expressions/variables/components."""
        graph = self._exprgraph

        to_remove = set()
        exprs = []

        if destpath is None:
            exprs = self.find_referring_exprs(srcpath)
            for expr in exprs:
                to_remove.update(graph.edges(expr))
                to_remove.update(graph.in_edges(expr))
        else:
            if srcpath in graph and destpath in graph:
                to_remove.add((srcpath, destpath))
            else:  # assume they're disconnecting two variables, so find connected exprs that refer to them
                src_exprs = set(self.find_referring_exprs(srcpath))
                dest_exprs = set(self.find_referring_exprs(destpath))
                to_remove.update([(src, dest) for src, dest in graph.edges()
                                               if src in src_exprs and dest in dest_exprs])

        added = []
        for src, dest in to_remove:
            try:
                pcomp = graph[src][dest]['pcomp']
            except KeyError:
                pass
            else:
                added.extend(pcomp.list_connections())

        to_remove.update(added)

        graph.remove_edges_from(to_remove)
        graph.remove_nodes_from(exprs)
        self._remove_disconnected_exprs()

        return to_remove

    def check_connect(self, src, dest, scope):
        """Check validity of connecting a source expression to a destination expression, and
        determine if we need to create links to pseudocomps.
        """

        if self.get_source(dest) is not None:
            scope.raise_exception("'%s' is already connected to source '%s'" % 
                                  (dest, self.get_source(dest)), RuntimeError)

        destexpr = ConnectedExprEvaluator(dest, scope, is_dest=True)
        srcexpr = ConnectedExprEvaluator(src, scope, 
                                         getter='get_wrapped_attr')

        srccomps = srcexpr.get_referenced_compnames()
        destcomps = list(destexpr.get_referenced_compnames())

        if destcomps and destcomps[0] in srccomps:
            raise RuntimeError("'%s' and '%s' refer to the same component." % (src, dest))

        return srcexpr, destexpr, self._needs_pseudo(scope, srcexpr, destexpr)

    def _new_pseudo_name(self):
        name = "_%d" % self._pseudo_count
        self._pseudo_count += 1
        return name

    def _needs_pseudo(self, parent, srcexpr, destexpr):
        """Possibly create a pseudo-component if srcexpr and destexpr require it.
        Otherwise, return None.
        """
        srcrefs = list(srcexpr.refs())
        if srcrefs and srcrefs[0] != srcexpr.text:
            # expression is more than just a simple variable reference, so we need a pseudocomp
            return True

        destmeta = destexpr.get_metadata('units')
        srcmeta = srcexpr.get_metadata('units')

        srcunit = srcmeta[0][1] if srcmeta else None
        destunit = destmeta[0][1] if destmeta else None

        if destunit and srcunit and destunit != srcunit:
            return True

        return False

    def _make_pseudo(self, parent, srcexpr, destexpr):
        """Possibly create a pseudo-component if srcexpr and destexpr require it.
        Otherwise, return None.
        """
        pcomp = PseudoComponent(self._new_pseudo_name(), parent, srcexpr, destexpr)

        return pcomp






