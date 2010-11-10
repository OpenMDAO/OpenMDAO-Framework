""" Class definition for Assembly """


#public symbols
__all__ = ['Assembly']

import cStringIO

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Instance, TraitError, Missing
from enthought.traits.api import TraitType

from openmdao.main.container import find_trait_and_value
from openmdao.main.component import Component
from openmdao.main.driver import Driver
from openmdao.main.tvalwrapper import TraitValWrapper

_iodict = { 'out': 'output', 'in': 'input' }


class PassthroughTrait(TraitType):
    """A trait that can use another trait for validation, but otherwise is
    just a trait that lives on an Assembly boundary and can be connected
    to other traits within the Assembly.
    """

    def validate(self, obj, name, value):
        """Validation for the PassThroughTrait"""
        if self.validation_trait:
            return self.validation_trait.validate(obj, name, value)
        return value

class Assembly (Component):
    """This is a container of Components. It understands how to connect inputs
    and outputs between its children.  When executed, it runs the top level
    Driver called 'driver'.
    """
    
    driver = Instance(Driver, allow_none=True,
                      desc="The top level Driver that manages execution of "
                           "this Assembly")
    
    def __init__(self, doc=None, directory=''):
        super(Assembly, self).__init__(doc=doc, directory=directory)
        
        # default Driver executes its workflow once
        self.add('driver', Driver())
        
    def add(self, name, obj):
        """Add obj to the component graph and call base class *add*.
        
        Returns the added object.
        """
        obj = super(Assembly, self).add(name, obj)
        if isinstance(obj, Component):
            self._depgraph.add(obj.name)
        
        return obj
        
    def remove(self, name):
        """Remove the named container object from this assembly and remove
        it from its workflow (if any)."""
        cont = getattr(self, name)
        self._depgraph.remove(name)
        for obj in self.__dict__.values():
            if obj is not cont and isinstance(obj, Driver):
                obj.workflow.remove(cont)
                    
        return super(Assembly, self).remove(name)

    def create_passthrough(self, pathname, alias=None):
        """Creates a PassthroughTrait that uses the trait indicated by
        pathname for validation, adds it to self, and creates a connection
        between the two. If alias is *None,* the name of the alias trait will
        be the last entry in its pathname. The trait specified by pathname
        must exist.
        """
        if alias:
            newname = alias
        else:
            parts = pathname.split('.')
            newname = parts[-1]

        if newname in self.__dict__:
            self.raise_exception("'%s' already exists" %
                                 newname, TraitError)
        parts = pathname.split('.', 1)
        if len(parts) < 2:
            self.raise_exception('destination of passthrough must be a dotted path',
                                 NameError)
        comp = getattr(self, parts[0])
        trait = comp.get_trait(parts[1])
        if trait:
            iotype = trait.iotype
            ttype = trait.trait_type
            if ttype is None:
                ttype = trait
            
            if not trait.validate:
                trait = None  # no validate function, so just don't use trait for validation
        else:
            if not self.contains(pathname):
                self.raise_exception("the variable named '%s' can't be found" %
                                     pathname, TraitError)
            iotype = self.get_metadata(pathname, 'iotype')
            
        newtrait = PassthroughTrait(iotype=iotype, validation_trait=trait)
        self.add_trait(newname, newtrait)
        setattr(self, newname, self.get(pathname))

        if iotype == 'in':
            self.connect(newname, pathname)
        else:
            self.connect(pathname, newname)
            
        return newtrait

    def _split_varpath(self, path):
        """Return a tuple of compname,component,varname given a path
        name of the form 'compname.varname'. If the name is of the form 'varname'
        then compname will be None and comp is self. 
        """
        try:
            compname, varname = path.split('.', 1)
        except ValueError:
            return (None, self, path)
        
        return (compname, getattr(self, compname), varname)

    def connect(self, srcpath, destpath):
        """Connect one src Variable to one destination Variable. This could be
        a normal connection between variables from two internal Components, or
        it could be a passthrough connection, which connects across the scope boundary
        of this object.  When a pathname begins with 'parent.', that indicates
        that it is referring to a Variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable
            
        destpath: str
            Pathname of destination variable
        """
        srccompname, srccomp, srcvarname = self._split_varpath(srcpath)
        destcompname, destcomp, destvarname = self._split_varpath(destpath)
        
        if srccomp is not self.parent and destcomp is not self.parent:
            dest_io = 'out' if destcomp is self else 'in'
            src_io = 'in' if srccomp is self else 'out'
            
            srctrait = srccomp.get_dyn_trait(srcvarname, src_io)
            desttrait = destcomp.get_dyn_trait(destvarname, dest_io)
            
            if srccompname == destcompname:
                self.raise_exception(
                    'Cannot connect %s to %s. Both are on same component.' %
                                     (srcpath, destpath), RuntimeError)
    
            # test type compatability
            ttype = desttrait.trait_type
            if not ttype:
                ttype = desttrait
            try:
                if ttype.get_val_wrapper:
                    srcval = srccomp.get_wrapped_attr(srcvarname)
                else:
                    srcval = srccomp.get(srcvarname)
                if ttype.validate:
                    ttype.validate(destcomp, destvarname, srcval)
                else:
                    pass  # no validate function on destination trait. Most likely
                          # it's a property trait.  No way to validate without
                          # unknown side effects.
            except TraitError, err:
                self.raise_exception("can't connect '%s' to '%s': %s" %
                                     (srcpath, destpath, str(err)), TraitError)
                    
        super(Assembly, self).connect(srcpath, destpath)
        
        # if it's an internal connection, could change dependencies, so we have
        # to call config_changed to notify our driver
        if srccomp is not self.parent and destcomp is not self.parent:
            if srccomp is not self and destcomp is not self:
                self.config_changed(update_parent=False)

            outs = destcomp.invalidate_deps(varnames=set([destvarname]), force=True)
            if (outs is None) or outs:
                bouts = self.child_invalidated(destcompname, outs, force=True)

    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs. 
        """
        if varpath2 is None:
            for src,sink in self._depgraph.connections_to(varpath):
                if src.startswith('@'):
                    src = src.split('.',1)[1]
                if sink.startswith('@'):
                    sink = sink.split('.',1)[1]
                super(Assembly, self).disconnect(src, sink)
        else:
            super(Assembly, self).disconnect(varpath, varpath2)
            
    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed, connections are made
        or removed, etc.
        """
        super(Assembly, self).config_changed(update_parent)
        # driver must tell workflow that config has changed because
        # dependencies may have changed
        if self.driver is not None:
            self.driver.config_changed(update_parent=False)
        
    def execute (self):
        """Runs driver and updates our boundary variables."""
        self.driver.run()
        
        # now update boundary outputs
        valids = self._valid_dict
        for srccompname,link in self._depgraph.in_links('@bout'):
            srccomp = getattr(self, srccompname)
            for dest,src in link._dests.items():
                if valids[dest] is False:
                    setattr(self, dest, srccomp.get_wrapped_attr(src))
    
    def step(self):
        """Execute a single child component and return."""
        self.driver.step()
        
    def stop(self):
        """Stop the calculation."""
        self.driver.stop()
    
    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        return self._depgraph.list_connections(show_passthrough)

    def _cvt_input_srcs(self, sources):
        link = self._depgraph.get_link('@xin', '@bin')
        if link is None:
            return sources
        else:
            newsrcs = []
            dests = link._dests
            for s in sources:
                if '.' in s:
                    newsrcs.append(dests[s])
                else:
                    newsrcs.append(s)
            return newsrcs
        
    def update_inputs(self, compname, varnames):
        """Transfer input data to input variables on the specified component.
        The varnames iterator is assumed to contain local names (no component name), 
        for example: ['a', 'b'].
        """
        parent = self.parent
        vset = set(varnames)
        if compname[0] == '@':
            destcomp = self
        else:
            destcomp = getattr(self, compname)
        for srccompname,srcs,dests in self._depgraph.in_map(compname, vset):
            if srccompname == '@bin':   # boundary inputs
                invalid_srcs = [s for s in srcs if not self._valid_dict[s]]
                if len(invalid_srcs) > 0:
                    if parent:
                        parent.update_inputs(self.name, invalid_srcs)
                    # invalid inputs have been updated, so mark them as valid
                    for name in invalid_srcs:
                        self._valid_dict[name] = True
                srccompname = ''
                srccomp = self
                srcs = self._cvt_input_srcs(srcs)
            else:
                srccomp = getattr(self, srccompname)
                if not srccomp.is_valid():
                    srccomp.update_outputs(srcs)
            
            # TODO: add multiget/multiset stuff here...
            for src,dest in zip(srcs, dests):
                try:
                    srcval = srccomp.get_wrapped_attr(src)
                except Exception, err:
                    self.raise_exception(
                        "error retrieving value for %s from '%s'" %
                        (src,srccompname), type(err))
                try:
                    if srccomp is self:
                        srcname = src
                    else:
                        srcname = '.'.join([srccompname, src])
                    if destcomp is self:
                        setattr(destcomp, dest, srcval)
                    else:
                        #destcomp.set(dest, srcval, src='parent.'+srcname)
                        # don't need to do source checking here unless we've messed up our bookkeeping
                        destcomp.set(dest, srcval, force=True)
                except Exception, exc:
                    if compname[0] == '@':
                        dname = dest
                    else:
                        dname = '.'.join([compname, dest])
                    msg = "cannot set '%s' from '%s': %s" % (dname, srcname, exc)
                    self.raise_exception(msg, type(exc))
            
    def update_outputs(self, outnames):
        """Execute any necessary internal or predecessor components in order
        to make the specified output variables valid.
        """
        simple, compmap = _partition_names_by_comp(outnames)
        if simple:  # boundary outputs
            self.update_inputs('@bout', simple)
        for cname, vnames in compmap.items():  # auto passthroughs to internal variables
            getattr(self, cname).update_outputs(vnames)
            self.set_valid(vnames, True)
        
    def get_valid(self, names):
        """Returns a list of boolean values indicating whether the named
        variables are valid (True) or invalid (False). Entries in names may
        specify either direct traits of self or those of children.
        """
        sup = super(Assembly,self)
        ret = []
        for name in names:
            cname, _, vname = name.partition('.')
            if vname:
                comp = getattr(self, cname)
                ret.extend(comp.get_valid(vname))
            else:
                ret.extend(sup.get_valid(name))
        return ret

    def check_resolve(self, names):
        """Returns True if all of the pathnames are resolvable starting from this
        Assembly.
        """
        simple, compmap = _partition_names_by_comp(names)
        for name in simple:
            if not hasattr(self, name):
                return False
        for cname,vnames in compmap.items():
            comp = getattr(self, cname, None)
            if comp is None:
                return False
            for vname in vnames:
                if not comp.contains(vname):
                    return False
        return True

    def _input_updated(self, name):
        if self._valid_dict[name]:  # if var is not already invalid
            outs = self.invalidate_deps(varnames=set([name]))
            if ((outs is None) or outs) and self.parent:
                self.parent.child_invalidated(self.name, outs)
            
    def child_invalidated(self, childname, outs=None, force=False):
        """Invalidate all variables that depend on the outputs provided
        by the child that has been invalidated.
        """
        bouts = self._depgraph.invalidate_deps(self, [childname], [outs], force)
        if bouts and self.parent:
            self.parent.child_invalidated(self.name, bouts, force)
        return bouts
                    
    def invalidate_deps(self, varnames=None, force=False):
        """Mark all Variables invalid that depend on varnames. 
        Returns a list of our newly invalidated boundary outputs.
        
        varnames: iter of str, optional
            An iterator of names of destination variables.
            
        force: bool, optional
            If True, force the invalidation to proceed beyond the 
            boundary even if all outputs were already invalid.
        """
        valids = self._valid_dict
        conn_ins = set(self.list_inputs(connected=True))
        
        # If varnames is None, we're being called from a parent Assembly
        # as part of a higher level invalidation, so we only need to look
        # at our connected inputs
        if varnames is None:
            names = conn_ins
        else:
            names = varnames
        
        # We only care about inputs that are changing from valid to invalid.
        # If they're already invalid, then we've already done what we needed to do,
        # unless force is True, in which case we continue with the invalidation.
        if force:
            invalidated_ins = names
        else:
            invalidated_ins = [n for n in names if valids[n] is True]
            if not invalidated_ins:  # no newly invalidated inputs, so no outputs change status
                return []

        if varnames is None:
            self.set_valid(invalidated_ins, False)
        else: # only invalidate *connected* inputs, because unconnected inputs
              # are always valid
            self.set_valid([n for n in invalidated_ins if n in conn_ins], False)

        outs = self._depgraph.invalidate_deps(self, ['@bin'], [invalidated_ins], force)

        if outs:
            self.set_valid(outs, False)
        return outs
    
    def exec_counts(self, compnames):
        return [getattr(self,c).exec_count for c in compnames]


def dump_iteration_tree(obj):
    """Returns a text version of the iteration tree
    of an OpenMDAO object or hierarchy.  The tree
    shows which are being iterated over by which
    drivers.
    """
    def _dump_iteration_tree(obj, f, tablevel):
        if isinstance(obj, Driver):
            f.write(' '*tablevel)
            f.write(obj.get_pathname())
            f.write('\n')
            for comp in obj.workflow:
                if isinstance(comp, Driver) or isinstance(comp, Assembly):
                    _dump_iteration_tree(comp, f, tablevel+3)
                else:
                    f.write(' '*(tablevel+3))
                    f.write(comp.get_pathname())
                    f.write('\n')
        elif isinstance(obj, Assembly):
            f.write(' '*tablevel)
            f.write(obj.get_pathname())
            f.write('\n')
            _dump_iteration_tree(obj.driver, f, tablevel+3)
    f = cStringIO.StringIO()
    _dump_iteration_tree(obj, f, 0)
    return f.getvalue()


def _partition_names_by_comp(names):
    """Take an iterator of names and return a tuple of the form (namelist, compmap)
    where namelist is a list of simple names (no dots) and compmap is a dict with component names
    keyed to lists of variable names.  
    """
    simple = []
    compmap = {}
    for name in names:
        parts = name.split('.', 1)
        if len(parts) == 1:
            simple.append(name)
        else:
            compmap.setdefault(parts[0], []).append(parts[1])
    return (simple, compmap)


