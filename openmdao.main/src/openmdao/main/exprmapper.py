import weakref

import networkx as nx

from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.units import PhysicalQuantity


class ExprMapper(object):
    """A mapping between source expressions and destination expressions"""
    def __init__(self, scope):
        self._exprgraph = nx.DiGraph()  # graph of source expressions to destination expressions
        self._scope = None if scope is None else weakref.ref(scope)

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_scope'] = self.scope
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        scope = state['_scope']
        self._scope = None if scope is None else weakref.ref(scope)

    @property
    def scope(self):
        return None if self._scope is None else self._scope()

    def get_expr(self, text):
        node = self._exprgraph.node.get(text)
        if node:
            return node['expr']
        return None

    def list_connections(self, show_passthrough=True, visible_only=False):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        lst = self._exprgraph.edges(data=True)

        if not show_passthrough:
            lst = [(u, v, data) for u, v, data in lst if '.' in u and '.' in v]

        if visible_only:
            newlst = []
            scope = self.scope
            for u, v, data in lst:
                pcomp = data.get('pcomp')
                if pcomp is not None:
                    newlst.extend(pcomp.list_connections(is_hidden=True))
                else:
                    srccmp = getattr(scope, u.split('.', 1)[0], None)
                    dstcmp = getattr(scope, v.split('.', 1)[0], None)
                    if isinstance(srccmp, PseudoComponent) or \
                       isinstance(dstcmp, PseudoComponent):
                        continue
                    newlst.append((u, v))
            return newlst

        return [(u, v) for u, v, data in lst]

    def get_source(self, dest_expr):
        """Returns the text of the source expression that is connected to the
        given destination expression.
        """
        dct = self._exprgraph.pred.get(dest_expr)
        if dct:
            return dct.keys()[0]
        else:
            return None

    def get_dests(self, src_expr):
        """Returns the list of destination expressions that are connected to
        the given source expression.
        """
        graph = self._exprgraph
        return [graph.node(name)['expr']
                for name in self._exprgraph.succ[src_expr].keys()]

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

        destcompname, destcomp, destvarname = _split_varpath(scope, destvar)
        desttrait = None
        srccomp = None

        if not isinstance(destcomp, PseudoComponent) and \
           not destvar.startswith('parent.') and not len(srcvars) > 1:
            for srcvar in srcvars:
                if not srcvar.startswith('parent.'):
                    srccompname, srccomp, srcvarname = _split_varpath(scope, srcvar)
                    if not isinstance(srccomp, PseudoComponent):
                        src_io = 'in' if srccomp is scope else 'out'
                        srccomp.get_dyn_trait(srcvarname, src_io)
                        if desttrait is None:
                            dest_io = 'out' if destcomp is scope else 'in'
                            desttrait = destcomp.get_dyn_trait(destvarname, dest_io)

                if not isinstance(srccomp, PseudoComponent) and \
                   desttrait is not None:
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
                            # unknown side effects. Have to wait until later
                            # when data actually gets passed via the connection.
                            pass

        if src not in self._exprgraph:
            self._exprgraph.add_node(src, expr=srcexpr)
        if dest not in self._exprgraph:
            self._exprgraph.add_node(dest, expr=destexpr)

        self._exprgraph.add_edge(src, dest)
        if pseudocomp is not None:
            self._exprgraph[src][dest]['pcomp'] = pseudocomp

    def find_referring_exprs(self, name):
        """Returns a list of expression strings that reference the given name,
        which can refer to either a variable or a component.
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
        """Disconnect the given expressions/variables/components.
        Returns a list of edges to remove and a list of pseudocomponents
        to remove.
        """
        graph = self._exprgraph

        to_remove = set()
        exprs = []
        pcomps = set()

        if destpath is None:
            exprs = self.find_referring_exprs(srcpath)
            for expr in exprs:
                to_remove.update(graph.edges(expr))
                to_remove.update(graph.in_edges(expr))
        else:
            if srcpath in graph and destpath in graph:
                to_remove.add((srcpath, destpath))
                data = graph[srcpath][destpath]
                if 'pcomp' in data:
                    pcomps.add(data['pcomp'].name)
            else:
                # assume they're disconnecting two variables, so find connected
                # exprs that refer to them
                src_exprs = set(self.find_referring_exprs(srcpath))
                dest_exprs = set(self.find_referring_exprs(destpath))
                to_remove.update([(src, dest) for src, dest in graph.edges()
                                               if src in src_exprs and dest in dest_exprs])

        added = []
        scope = self.scope
        for src, dest in to_remove:
            if src.startswith('_pseudo_'):
                pcomp = getattr(scope, src.split('.', 1)[0])
            elif dest.startswith('_pseudo_'):
                pcomp = getattr(scope, dest.split('.', 1)[0])
            else:
                continue
            added.extend(pcomp.list_connections())
            pcomps.add(pcomp.name)

        to_remove.update(added)

        graph.remove_edges_from(to_remove)
        graph.remove_nodes_from(exprs)
        self._remove_disconnected_exprs()

        return to_remove, pcomps

    def check_connect(self, src, dest, scope):
        """Check validity of connecting a source expression to a destination
        expression, and determine if we need to create links to pseudocomps.
        """

        if self.get_source(dest) is not None:
            scope.raise_exception("'%s' is already connected to source '%s'" %
                                  (dest, self.get_source(dest)), RuntimeError)

        destexpr = ConnectedExprEvaluator(dest, scope, is_dest=True)
        srcexpr = ConnectedExprEvaluator(src, scope,
                                         getter='get_attr_w_copy')

        srccomps = srcexpr.get_referenced_compnames()
        destcomps = list(destexpr.get_referenced_compnames())

        if destcomps and destcomps[0] in srccomps:
            raise RuntimeError("'%s' and '%s' refer to the same component."
                               % (src, dest))

        return srcexpr, destexpr, self._needs_pseudo(scope, srcexpr, destexpr)

    def _needs_pseudo(self, parent, srcexpr, destexpr):
        """Return a non-None pseudo_type if srcexpr and destexpr require a
        pseudocomp to be created.
        """
        srcrefs = list(srcexpr.refs())
        if srcrefs and srcrefs[0] != srcexpr.text:
            # expression is more than just a simple variable reference,
            # so we need a pseudocomp
            return 'multi_var_expr'

        destmeta = destexpr.get_metadata('units')
        srcmeta = srcexpr.get_metadata('units')

        # compare using get_unit_name() to account for unit aliases
        if srcmeta:
            srcunit = srcmeta[0][1]
            if srcunit:
                srcunit = PhysicalQuantity(1., srcunit).unit
        else:
            srcunit = None

        if destmeta:
            destunit = destmeta[0][1]
            if destunit:
                destunit = PhysicalQuantity(1., destunit).unit
        else:
            destunit = None

        if destunit and srcunit:
            if destunit.powers != srcunit.powers or \
               destunit.factor != srcunit.factor or \
               destunit.offset != srcunit.offset:
                return 'units'

        return None

    def list_pseudocomps(self):
        return [data['pcomp'].name for u, v, data in
                           self._exprgraph.edges(data=True) if 'pcomp' in data]


def _split_varpath(cont, path):
    """Return a tuple of compname,component,varname given a path
    name of the form 'compname.varname'. If the name is of the form
    'varname', then compname will be None and comp is cont.
    """
    try:
        compname, varname = path.split('.', 1)
    except ValueError:
        return (None, cont, path)

    t = cont.get_trait(compname)
    if t and t.iotype:
        return (None, cont, path)
    return (compname, getattr(cont, compname), varname)

