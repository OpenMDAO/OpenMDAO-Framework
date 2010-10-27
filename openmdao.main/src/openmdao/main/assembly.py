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
from openmdao.main.expression import Expression, ExpressionList
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
                obj.remove_from_workflow(cont)
                    
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
        trait, val = find_trait_and_value(self, pathname)
        if not trait:
            self.raise_exception("the variable named '%s' can't be found" %
                                 pathname, TraitError)
        iotype = trait.iotype
        # the trait.trait_type stuff below is for the case where the trait is actually
        # a ctrait (very common). In that case, trait_type is the actual underlying
        # trait object
        if (getattr(trait,'get') or getattr(trait,'set') or
            getattr(trait.trait_type, 'get') or getattr(trait.trait_type,'set')):
            trait = None  # not sure how to validate using a property
                          # trait without setting it, so just don't use it
        newtrait = PassthroughTrait(iotype=iotype, validation_trait=trait)
        self.add_trait(newname, newtrait)
        setattr(self, newname, val)

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
        
        srctrait = srccomp.find_trait(srcvarname)
        desttrait = destcomp.find_trait(destvarname)
        
        if srccompname == destcompname:
            self.raise_exception(
                'Cannot connect %s to %s. Both are on same component.' %
                                 (srcpath, destpath), RuntimeError)
        if srctrait and desttrait and (srctrait.is_trait_type and (srctrait.is_trait_type(Expression) or srctrait.is_trait_type(ExpressionList))) or \
           (desttrait.is_trait_type and (desttrait.is_trait_type(Expression) or desttrait.is_trait_type(ExpressionList))):
            self.raise_exception('Cannot connect %s to %s because one of them is an Expression or ExpressionList' %
                                 (srcpath,destpath), RuntimeError)

        if srccomp is not self and destcomp is not self:
            # it's not a passthrough, so must connect input to output
            if srctrait.iotype != 'out':
                self.raise_exception(
                    '.'.join([srccomp.get_pathname(),srcvarname])+
                    ' must be an output variable',
                    RuntimeError)
            if desttrait.iotype != 'in':
                self.raise_exception(
                    '.'.join([destcomp.get_pathname(),destvarname])+
                    ' must be an input variable',
                    RuntimeError)

        # test type compatability
        try:
            if desttrait.trait_type.get_val_wrapper:
                desttrait.validate(destcomp, destvarname,
                                   srccomp.get_wrapped_attr(srcvarname))
            else:
                desttrait.validate(destcomp, destvarname,
                                   srccomp.get(srcvarname))
        except TraitError, err:
            self.raise_exception("can't connect '%s' to '%s': %s" %
                                 (srcpath,destpath,str(err)), TraitError)
            
        super(Assembly, self).connect(srcpath, destpath)

        # invalidate destvar if necessary
        if destcomp is self or destcompname=='parent': # boundary output
            if destcomp.get_valid([destvarname])[0] and \
               srccomp.get_valid([srcvarname])[0] is False:
                if self.parent:
                    # tell the parent that anyone connected to our boundary
                    # output is invalid.
                    # Note that a boundary output is a dest var in this scope, 
                    # but a src var in the parent scope.
                    self.parent.child_invalidated(childname=self.name, outs=set([destvarname]))
        else:
            outs = destcomp.invalidate_deps(varnames=set([destvarname]))
            if (outs is None) or outs:
                bouts = self.child_invalidated(destcompname, outs)

        

    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs. 
        """
        to_remove = []
        if varpath in self._depgraph: # varpath is a component name
            if varpath2 is not None:
                self.raise_exception("disconnect: First arg '%s' is a Component name so the second arg '%s' is not allowed." %
                                     (varpath,varpath2), RuntimeError)
            for u,v in self._depgraph.var_edges(varpath):
                to_remove.append((u, v))
            for u,v in self._depgraph.var_in_edges(varpath):
                to_remove.append((u, v))
            
        else:   # varpath is not a component name
            if '.' not in varpath:
                if varpath in self.list_inputs():
                    varpath = '.'.join(['@exin', varpath])
                else:
                    varpath = '.'.join(['@exout', varpath])
            if varpath2 is not None and '.' not in varpath2:
                if varpath2 in self.list_inputs():
                    varpath2 = '.'.join(['@exin', varpath2])
                else:
                    varpath2 = '.'.join(['@exout', varpath2])
            
            cname, vname = varpath.split('.', 1)
            if varpath2 is None:  # remove all connections to varpath
                dotvname = '.'+vname
                for u,v in self._depgraph.var_edges(cname):
                    if u.endswith(dotvname):
                        to_remove.append((u, v))
                for u,v in self._depgraph.var_in_edges(cname):
                    if v.endswith(dotvname):
                        to_remove.append((u, v))
            else:
                to_remove.append((varpath, varpath2))

        for src,sink in to_remove:
            super(Assembly, self).disconnect(src, sink)
            
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
            if srccompname[0] == '@':   # boundary inputs
                invalid_srcs = []
                if srccompname == '@bin':
                    invalid_srcs = [s for s in srcs if self._valid_dict.get(s) is False]
                else:   # srccompname == '@exin':  
                    for d in dests:
                        pname = '.'.join([compname,d])
                        if self._valid_dict.get(pname) is False:
                            invalid_srcs.append(pname)
                if len(invalid_srcs) > 0:
                    if parent:
                        parent.update_inputs(self.name, invalid_srcs)
                    # invalid inputs have been updated, so mark them as valid
                    for name in invalid_srcs:
                        self._valid_dict[name] = True
                srccompname = ''
                srccomp = self
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
                        destcomp.set(dest, srcval, src='parent.'+srcname)
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
        fakes = [n for n in outnames if '.' in n]
        reals = [n for n in outnames if '.' not in n]
        if reals:  # 'real' boundary outputs
            self.update_inputs('@bout', reals)
        if fakes:  # 'fake' boundary outputs  (passthroughs to internal variables)
            compmap = {}
            for name in fakes:
                cname, _, varname = name.partition('.')
                if cname in compmap:
                    compmap[cname].append(varname)
                else:
                    compmap[cname] = [varname]
            for cname, vnames in compmap.items():
                getattr(self, cname).update_outputs(vnames)
                self.set_valid(vnames, True)
        
    def get_valid(self, names):
        """Returns a list of boolean values indicating whether the named
        variables are valid (True) or invalid (False). Entries in names may
        specify either direct traits of self or those of direct children of
        self, but no deeper in the hierarchy than that.
        """
        simple, compmap = _partition_names_by_comp(names)
        ret = super(Assembly, self).get_valid(simple)
        for cname, vnames in compmap.items():
            comp = getattr(self, cname)
            ret.extend(comp.get_valid(vnames))
        return ret

    def check_resolve(self, pathnames):
        """Returns True if all of the pathnames are resolvable starting from this
        Assembly.
        """
        for name in pathnames:
            tup = name.split('.', 1)
            if len(tup) > 1:
                comp = getattr(self, tup[0], None)
                if comp is None or not comp.contains(tup[1]):
                    return False
            elif not hasattr(self, name):
                return False
        return True

    def _input_updated(self, name):
        if self._valid_dict[name]:  # if var is not already invalid
            outs = self.invalidate_deps(varnames=set([name]))
            if ((outs is None) or outs) and self.parent:
                self.parent.child_invalidated(self.name, outs)
            
    def child_invalidated(self, childname, outs=None):
        """Invalidate all variables that depend on the outputs provided
        by the child that has been invalidated.
        """
        bouts = self._depgraph.invalidate_deps(self, [childname], [outs])
        if bouts and self.parent:
            self.parent.child_invalidated(self.name, bouts)

        return bouts
                    
    def invalidate_deps(self, varnames=None):
        """Mark all Variables invalid that depend on varnames. 
        Returns a list of our newly invalidated boundary outputs.
        """
        if varnames is None:
            varnames = self.get_connected_inputs()

        # @exin is a little weird and we have to swap varnames for
        # its sources that map to the varnames as destinations
        newnames = set()
        found = set()
        
        for destcomp, link in self._depgraph.out_links('@exin'):
            for src,dests in link._srcs.items():
                for dest in dests:
                    if destcomp == '@bin':
                        if dest in varnames:
                            newnames.add(src)
                            found.add(dest)
                    else:
                        name = '.'.join([destcomp,dest])
                        if name in varnames:
                            newnames.add(src)
                            found.add(name)
                            
        if found:
            self.set_valid(found, False)
        
        outs = set()
        if newnames:
            outs.update(self._depgraph.invalidate_deps(self, ['@exin'], [newnames]))
        orignames = set(varnames)-found
        if orignames:
            outs.update(self._depgraph.invalidate_deps(self, ['@bin'], [orignames]))
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


