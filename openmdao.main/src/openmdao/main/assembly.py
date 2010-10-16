""" Class definition for Assembly """


#public symbols
__all__ = ['Assembly']

import cStringIO

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Instance, TraitError
from enthought.traits.api import TraitType

from openmdao.main.container import find_trait_and_value, get_trait
from openmdao.main.component import Component
from openmdao.main.driver import Driver
from openmdao.main.expression import Expression, ExpressionList

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
            self._depgraph.add(obj)
        
        return obj
        
    def remove(self, name):
        """Remove the named container object from this assembly and remove
        it from its workflow (if any)."""
        cont = getattr(self, name)
        self._depgraph.remove(cont)
        for obj in self.__dict__.values():
            if obj is not cont and isinstance(obj, Driver):
                obj.remove_from_workflow(cont)
                    
        return super(Assembly, self).remove(name)


    def create_passthrough(self, pathname, alias=None):
        """Creates a PassthroughTrait that uses the trait indicated by
        pathname for validation (if it's not a property trait), adds it to
        self, and creates a connection between the two. If alias is *None,*
        the name of the "promoted" trait will be the last entry in its
        pathname. The trait specified by pathname must exist.
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

    #def get_dyn_trait(self, pathname, iotype):
        #"""Retrieves the named trait, attempting to create a PassthroughTrait
        #on-the-fly if the specified trait doesn't exist.
        #"""
        #trait = get_trait(self, pathname)
        #if trait is None:
            #trait = self.create_passthrough(pathname)
        #return trait

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

    def connect(self, srcpath, destpath, value=None):
        """Connect one src Variable to one destination Variable. This could be
        a normal connection between variables from two internal Components, or
        it could be a passthrough connection, which connects across the scope boundary
        of this object.  When a pathname begins with 'parent.', that indicates
        that it is referring to a Variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable
            
        destpath: str
            Pathname of destination variable
            
        value: object, optional
            A value used for validation by the destination variable
        """

        super(Assembly, self).connect(srcpath, destpath, value)
        
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
            if srctrait and srctrait.iotype != 'out':
                self.raise_exception(
                    '.'.join([srccomp.get_pathname(),srcvarname])+
                    ' must be an output variable',
                    RuntimeError)
            if desttrait and desttrait.iotype != 'in':
                self.raise_exception(
                    '.'.join([destcomp.get_pathname(),destvarname])+
                    ' must be an input variable',
                    RuntimeError)
                
        #sname = self._depgraph.get_source(destcompname, destvarname)
        #if sname is not None:
            #self.raise_exception('%s is already connected to %s' % (destpath, sname),
                                 #RuntimeError)             
            
        # test compatability (raises TraitError on failure)
        if desttrait and desttrait.validate is not None:
            try:
                if desttrait.trait_type.get_val_meta_wrapper:
                    desttrait.validate(destcomp, destvarname, 
                                       srccomp.get_wrapped_attr(srcvarname))
                else:
                    desttrait.validate(destcomp, destvarname, 
                                       srccomp.get(srcvarname))
            except TraitError, err:
                self.raise_exception("can't connect '%s' to '%s': %s" % 
                                     (srcpath,destpath,str(err)), TraitError)
            
        #if destcomp is not self and srccomp is not self: # neither var is on boundary
            #self._depgraph.connect(srcpath, destpath)
        
        # invalidate destvar if necessary
        if destcomp is self and desttrait and desttrait.iotype == 'out': # boundary output
            if destcomp.get_valid(destvarname) and \
               srccomp.get_valid(srcvarname) is False:
                if self.parent:
                    # tell the parent that anyone connected to our boundary
                    # output is invalid.
                    # Note that it's a dest var in this scope, but a src var in
                    # the parent scope.
                    self.parent.invalidate_deps(self.name, [destvarname], True)
            #self._depgraph.connect(srcpath, '.'.join(['@out',destvarname]))
            self._valid_dict[destpath] = False
        elif srccomp is self and srctrait.iotype == 'in': # boundary input
            #self._depgraph.connect('.'.join(['@in',srcpath]), destpath)
            pass
        else:
            destcomp.invalidate_deps(varnames=[destvarname], notify_parent=True)

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
                    varpath = '.'.join(['@in', varpath])
                else:
                    varpath = '.'.join(['@out', varpath])
            if varpath2 is not None and '.' not in varpath2:
                if varpath2 in self.list_inputs():
                    varpath2 = '.'.join(['@in', varpath2])
                else:
                    varpath2 = '.'.join(['@out', varpath2])
            
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
            #sinkcomp,sinkvar = sink.split('.', 1)
            #if sinkcomp[0] != '@':  # sink is not on boundary
                #getattr(self, sinkcomp).disconnect(sinkvar)
            #self._depgraph.disconnect(src, sink)

    #def set_source(self, destname, srcname):
        #"""Mark the named io trait as a destination by registering a source
        #for it, which will prevent it from being set directly or connected 
        #to another source.
        
        #destname: str
            #Name of the destination variable.
            
        #srcname: str
            #Pathname of the source variable. The pathname may contain references
            #to 'parent.' indicating that the source is from outside of the 
            #immediate parent's scope.
            
        #"""
        #super(Assembly, self).set_source(destname, srcname)
        #if '.' in destname:
            #self._depgraph.connect('@in.%s' % srcname, destname)
            
    def execute (self):
        """Runs driver and updates our boundary variables."""
        self.driver.run()
        valids = self._valid_dict
        self._update_boundary_vars()
    
    def _update_boundary_vars (self):
        """Update output variables on our boundary."""
        valids = self._valid_dict
        for srccompname,link in self._depgraph.in_links('@self'):
            if srccompname == '@in':
                continue
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
        if compname == '@self':
            destcomp = self
        else:
            destcomp = getattr(self, compname)
        for srccompname,srcs,dests in self._depgraph.in_map(compname, vset):
            if srccompname[0] == '@':  # boundary inputs
                srccompname = ''
                srccomp = self
                invalid_srcs = [k for k,v in self._valid_dict.items() if v is False and k in srcs]
                if len(invalid_srcs) > 0:
                    if parent:
                        parent.update_inputs(self.name, invalid_srcs)
                    # invalid inputs have been updated, so make them as valid
                    for name in invalid_srcs:
                        self._valid_dict[name] = True
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
        self.update_inputs('@self', outnames)
        
    def get_valids(self, names):
        """Returns a list of boolean values indicating whether the named
        variables are valid (True) or invalid (False). Entries in names may
        specify either direct traits of self or those of direct children of
        self, but no deeper in the hierarchy than that.
        """
        valids = []
        vdict = self._valid_dict
        for name in names:
            v = vdict.get(name, None)
            if v is not None:
                valids.append(v)
            else:
                tup = name.split('.', 1)
                if len(tup) > 1:
                    comp = getattr(self, tup[0])
                    valids.append(comp.get_valid(tup[1]))
                else:
                    self.raise_exception("get_valids: unknown variable '%s'" %
                                         name, RuntimeError)
        return valids

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

    def invalidate_deps(self, compname=None, varnames=None, notify_parent=False):
        """Mark all Variables invalid that depend on varnames. 
        Returns a list of our newly invalidated boundary outputs.
        """
        outs = set()
        compgraph = self._depgraph
        if compname is None: # check boundary inputs
            compname = '@self'
            if varnames is not None:
                for name in self._depgraph.get_connected_inputs():
                    if name in varnames:
                        self._valid_dict[name] = False
        visited = set()
        partial_visited = {}
        stack = [(compname, varnames)]
            
        while len(stack) > 0:
            cname, vnames = stack.pop()
            if vnames is None:
                for destcname, link in compgraph.out_links(cname):
                    if link in visited: continue
                    
                    if destcname[0] == '@':
                        outs.update(link.get_dests())
                    else:
                        outnames = getattr(self, destcname).invalidate_deps(varnames=link.get_dests())
                        stack.append((destcname, outnames))
                    visited.add(link)
            else:  # specific varnames are invalidated
                for destcname, link in compgraph.out_links(cname):
                    if link in visited: continue
                    inputs = link.get_dests(varnames)
                    if not inputs: continue
                    if destcname == '@out':
                        outs.update(inputs)
                    else:
                        outnames = getattr(self, destcname).invalidate_deps(varnames=inputs)
                        stack.append((destcname, outnames))
                    partial = partial_visited.setdefault(link, set())
                    partial.update(inputs)
                    if len(partial) == len(link._srcs):
                        visited.add(link)
                        del partial_visited[link]
        
        if len(outs) > 0:
            self.set_valids(outs, False)
            if notify_parent and self.parent:
                self.parent.invalidate_deps(compname=self.name, varnames=outs, notify_parent=True)
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


def asm_dump(asm):
    def _asm_dump(asm, f):
        f.write('inputs: %s\n' % asm.list_inputs())
        f.write('outputs: %s\n' % asm.list_outputs())
        f.write('boundary ins: \n')
        for name in asm._depgraph.get_connected_inputs():
            f.write('   %s\n' % src)
        f.write('boundary outs: \n')
        for name in asm._depgraph.get_connected_outputs():
            f.write('   %s\n' % dest)
        f.write('valids: %s\n' % asm._valid_dict)
    f = cStringIO.StringIO()
    _asm_dump(asm, f)
    return f.getvalue()
    