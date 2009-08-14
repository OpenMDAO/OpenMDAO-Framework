
__all__ = ('NPSScomponent','NPSSProperty')


import os

import numpy
from enthought.traits.api import TraitType, Array, Bool, Float, Dict, Int, \
                                 Str, List, Undefined, TraitError, Instance, Python
from enthought.traits.trait_handlers import NoDefaultSpecified

from openmdao.main.api import Component, FileTrait, FileValue, UnitsFloat

import npss
npss.isolateContexts(True)

import units

_iodict = { 'input':'in', 'output':'out', 'unset':'in' }

_excludes = set(['type'])


class NPSSProperty(TraitType):
    """A Trait that sets/gets values within an NPSS model.
    """
    def __init__ ( self, default_value = NoDefaultSpecified, **metadata ):
        trait = metadata.get('trait', None)
        if trait is not None:
            for name,val in trait._metadata.items():
                if name not in _excludes:
                    metadata.setdefault(name, val)
        super(NPSSProperty, self).__init__(default_value, **metadata)

    def get(self, obj, name):
        """Return the NPSS value specified in the ref_name attribute."""
        trait = self.trait
        # FIXME: pull the file stuff out of here and fix it up
        if trait and isinstance(trait, FileTrait):
            meta = self.trait._metadata.copy()
            meta.pop('iostatus')
            meta['filename'] = getattr(obj._top, self.ref_name+'.filename')
            return FileValue(**meta)
        else:
            return getattr(obj._top, self.ref_name or name)

    def set(self, obj, name, value):
        """Set the NPSS value specified in the ref_name attribute."""
        if self.iostatus == 'out':
            raise TraitError('%s is an output trait and cannot be set' % name)
        
        if self.trait:
            value = self.trait.validate(obj, name, value)
            # FIXME: pull the file stuff out of here and fix it up
            if isinstance(value, FileValue):
                setattr(obj._top, self.ref_name+'.filename', value.filename)
                return
        setattr(obj._top, self.ref_name or name, value)
        
        
class NPSScomponent(Component):
    """
    An NPSS wrapper component.  Supports reload requests either internally
    via a flag in the model or externally by setting the reload_model flag.

    arglist is a list of arguments just like the command-line arguments to
    NPSS.  If arglist is a string, it will be split on whitespace to create
    a list.  See the NPSS User Guide for more information.

    If outfile is non-null, NPSS generated output will be routed to that file.

    If top is non-null, it is taken as the name of the 'top' object.

    NOTE: returned arrays are a *copy* of the NPSS data.

    .. parsed-literal::

        TODO: Support index argument to get() and set().
        TODO: Detect & flag execution errors.
        TODO: Save model state as well as component state.
        TODO: Use buffer protocol for array access.
        TODO: Safe multi-thread access (synchronized).
        TODO: CORBA support.
        TODO: function/table attribute support.

    """
    
    # Model options.
    model_filename = Str(iostatus='in',
                         desc='Filename for NPSS model.')
    include_dirs = List(str, iostatus='in',
                        desc='Model include directories.')
    use_default_paths = Bool(True, iostatus='in',
                             desc='Use default NPSS directories.')
    preprocessor_vars = Dict(str, str, iostatus='in',
                             desc='Preprocessor variable definitions')

    # Execution options.
    run_command = Str(iostatus='in',
                      desc='String to parse to run model.')
    reload_flag = Str(iostatus='in', 
                      desc='Path to flag to internally request a model reload.')

    preloaded_dlms = List(str, iostatus='in',
                          desc='Preloaded DLMs.')
    iclod_first = Bool(False, iostatus='in',
                       desc='Search ICLOD before DCLOD.')
    no_dclod = Bool(False, iostatus='in', 
                    desc='Do not search DCLOD.')
    no_iclod = Bool(False, iostatus='in',
                    desc='Do not search ICLOD.')

    use_corba = Bool(False, iostatus='in',
                     desc='Enable distributed simulation via CORBA.')

    # Output options.
    output_filename = Str(iostatus='in',
           desc='Filename for standard streams in all new sessions.')
    trace_execution = Bool(False, iostatus='in',
                           desc='Trace interpreted statement execution.')

    # Advanced options.
    assembly_type = Str(iostatus='in', desc='Type for top object.')
    executive_type = Str(iostatus='in', desc='Top-level executive.')
    preloaded_objs = List(str, iostatus='in', desc='Preloaded Objects.')
    use_solver = Bool(True, iostatus='in', desc='Use default solver.')
    use_constants = Bool(True, iostatus='in',
                         desc='Use default constants.')
    access = Str(iostatus='in', desc='Default access type.')
    autodoc = Bool(False, iostatus='in', 
                   desc='Allow abstract creation.')
    ns_ior = Str(iostatus='in', 
                    desc='IOR of NamingService.')
    other_opts = Str(iostatus='in', desc='Other options.')

    # Wrapper stuff.
    reload_model = Bool(False, iostatus='in', 
         desc='Flag to externally request a model reload.')
    
    __ = Python
    
    _ = NPSSProperty(scope='_top')
    
    # 1st session gets bad context for some reason...
    dummy = npss.npss()

    #name='NPSS'
    def __init__(self, doc=None, directory='',
                 arglist=None, output_filename='', top=''):
        super(NPSScomponent, self).__init__(doc, directory)
        if not isinstance(top, basestring):
            self.raise_exception('top must be a string', TypeError)
        self._topstr = top
        self.output_filename = output_filename
        self._arglist = arglist
        self._top = None
        

    def hierarchy_defined(self):
        """Performs checking of paths for any files/directories 
        supplied as args, in addition to calling the base class
        version of hierarchy_defined.
        """
        super(NPSScomponent, self).hierarchy_defined()
        
        if self._arglist is not None:
            self._parse_arglist(self._arglist)
        
        if self.output_filename and not os.path.isabs(self.output_filename):
            self.output_filename = os.path.join(self.get_abs_directory(),
                                                self.output_filename)
            
        self.reload()        
        
    def __getstate__(self):
        """ Return dict representing this Component's state. """
        state = super(NPSScomponent, self).__getstate__()
        state['_top'] = None  # pyNPSS is unpickleable.
        return state

    def __setstate__(self, state):
        """ Restore this Component's state. """
        super(NPSScomponent, self).__setstate__(state)
        # _top will be set during post_load via reload().

    def post_load(self):
        """ Perform any required operations after model has been loaded. """
        super(NPSScomponent, self).post_load()
        try:
            self.reload()
        except Exception, exc:
            self.raise_exception('Reload caught exception: %s' % exc,
                                 type(exc))

    def pre_delete(self):
        """ Perform any required operations before the model is deleted. """
        super(NPSScomponent, self).pre_delete()
        if self._top is not None:
            self._top.closeSession()
            self._top = None

    def _parse_arglist(self, arglist):
        """ Parse argument list. Assumes flag argument separate from value. """
        access_next = False
        assembly_next = False
        dlm_next = False
        executive_next = False
        include_next = False
        ns_next = False
        obj_next = False
        preproc_next = False

        if isinstance(arglist, basestring):
            args = arglist.split()
        else:
            try:
                args = iter(arglist)
            except TypeError, exc:
                self.raise_exception(exc.args[0], type(exc))

        for arg in args:
            if arg == '-C':
                obj_next = True
            elif obj_next:
                if self._check_path(arg):
                    self.preloaded_objs.append(arg)
                else:
                    self.raise_exception("object '%s' does not exist" % arg,
                                         ValueError)
                obj_next = False
            elif arg == '-D':
                preproc_next = True
            elif preproc_next:
                items = arg.split('=')
                name = items[0]
                if len(items) > 1:
                    value = items[1]
                else:
                    value = '1'
                self.preprocessor_vars[name] = value
                preproc_next = False
            elif arg == '-E':
                executive_next = True
            elif executive_next:
                self.executive_type = arg
                executive_next = False
            elif arg == '-I':
                include_next = True
            elif include_next:
                if self._check_path(arg):
                    self.include_dirs.append(arg)
                else:
                    self.raise_exception("include path '%s' does not exist" % arg,
                                         ValueError)
                include_next = False
            elif arg == '-X':
                assembly_next = True
            elif assembly_next:
                self.assembly_type = arg
                assembly_next = False
            elif arg == '-a':
                access_next = True
            elif access_next:
                if arg == 'PRIV' or arg == 'RO' or arg == 'RW':
                    self.access = arg
                else:
                    self.raise_exception("invalid access '%s'" % arg,
                                         ValueError)
                access_next = False
            elif arg == '-autodoc':
                self.autodoc = True
            elif arg == '-corba':
                self.use_corba = True
            elif arg == '-iclodfirst':
                self.iclod_first = True
            elif arg == '-l':
                dlm_next = True
            elif dlm_next:
                if self._check_path(arg):
                    self.preloaded_dlms.append(arg)
                else:
                    self.raise_exception("DLM path '%s' does not exist" % arg,
                                         ValueError)
                dlm_next = False
            elif arg == '-noconstants':
                self.use_constants = False
            elif arg == '-noDefPaths':
                self.use_default_paths = False
            elif arg == '-nodclod':
                self.no_dclod = True
            elif arg == '-noiclod':
                self.no_iclod = True
            elif arg == '-nosolver':
                self.use_solver = False
            elif arg == '-ns':
                ns_next = True
            elif ns_next:
                if self._check_path(arg):
                    self.ns_ior = arg
                else:
                    self.raise_exception("NameServer IOR path '%s' does not exist" % arg,
                                         ValueError)
                ns_next = False
            elif arg == '-trace':
                self.trace_execution = True
            elif arg.startswith('-'):
                self.raise_exception("illegal option '%s'" % arg, ValueError)
            else:
                if self._check_path(arg):
                    self.model_filename = arg
                else:
                    self.raise_exception("model file '%s' does not exist" % arg,
                                         ValueError)

        if access_next:
            self.raise_exception("expected default access type", ValueError)
        elif assembly_next:
            self.raise_exception("expected assembly type", ValueError)
        elif dlm_next:
            self.raise_exception("expected DLM path", ValueError)
        elif executive_next:
            self.raise_exception("expected executive type", ValueError)
        elif include_next:
            self.raise_exception("expected include path", ValueError)
        elif ns_next:
            self.raise_exception("expected NameServer IOR path", ValueError)
        elif obj_next:
            self.raise_exception("expected object path", ValueError)
        elif preproc_next:
            self.raise_exception("expected preprocessor value", ValueError)

    def _check_path(self, path):
        """ Return True if path can be found. """
        if os.path.isabs(path):
            return os.path.exists(path)
        else:
            return os.path.exists(os.path.join(self.get_abs_directory(), path))

    def _generate_arglist(self):
        """ Generate argument list. """
        arglist = []
        for obj in self.preloaded_objs:
            arglist.append('-C')
            arglist.append(obj)
        for name in self.preprocessor_vars.keys():
            arglist.append('-D')
            arglist.append('%s=%s' % (name, self.preprocessor_vars[name]))
        if self.executive_type:
            arglist.append('-E')
            arglist.append(self.executive_type)
        for path in self.include_dirs:
            arglist.append('-I')
            arglist.append(path)
        if self.assembly_type:
            arglist.append('-X')
            arglist.append(self.assembly_type)
        if self.access:
            arglist.append('-a')
            arglist.append(self.access)
        if self.autodoc:
            arglist.append('-autodoc')
        if self.use_corba:
            arglist.append('-corba')
        if self.iclod_first:
            arglist.append('-iclodfirst')
        for dlm in self.preloaded_dlms:
            arglist.append('-l')
            arglist.append(dlm)
        if not self.use_constants:
            arglist.append('-noconstants')
        if not self.use_default_paths:
            arglist.append('-noDefPaths')
        if self.no_dclod:
            arglist.append('-nodclod')
        if self.no_iclod:
            arglist.append('-noiclod')
        if not self.use_solver:
            arglist.append('-nosolver')
        if self.ns_ior:
            arglist.append('-ns')
            arglist.append(self.ns_ior)
        if self.trace_execution:
            arglist.append('-trace')
        return arglist

    def reload(self):
        """ (Re)load model. """
        is_reload = False
        saved_inputs = {}
        removed_traits = []
        if self._top is not None:
            is_reload = True
            # Save current values and trait info for attributes that are 
            # connected to a source
            for name in self._sources.keys():
                saved_inputs[name] = (self.get(name), 
                                      self.trait(name).ref_name or '', 
                                      self.trait(name).iostatus)
                
            # Remove model input files from external_files list.
            paths = self._top.inputFileList
            cwd = self.get_abs_directory()+os.sep
            for path in paths:
                if path.startswith(cwd):
                    path = path[len(cwd):]
                for i, meta in enumerate(self.external_files):
                    if meta['path'] == path:
                        self.external_files.pop(i)
                        break
            self._top.closeSession()
            self._top = None
            
            # remove all of the old dynamically added traits
            for name, value in self._added_traits.items():
                removed_traits.append((name, value.ref_name or '',
                                       value.iostatus))
                self.remove_trait(name)

        # Default session directory is set during initialization.
        directory = self.get_abs_directory()
        if not os.path.exists(directory):
            raise RuntimeError("Execution directory '%s' not found."
                               % directory)
        self.push_dir(directory)
        try:
            cwd = os.getcwd()+os.sep
            if is_reload:
                self.info('Reloading session in %s', cwd)
            arglist = self._generate_arglist()
            if self.output_filename:
                if not '-singleStream' in arglist:
                    arglist.insert(0, '-singleStream')

            self._top = npss.npss(arglist=arglist, top=self._topstr)

            if self.output_filename:
                self._top.cout.append = is_reload
                self._top.cout.filename = self.output_filename
                if is_reload:
                    msg = '\nReloading session in '+cwd+'\n'
                    if self.model_filename:
                        msg += 'Model filename '+self.model_filename+'\n'
                    self._top.cout.println(msg)
                self.info('output routed to %s', self.output_filename)

            if self.model_filename:
                if not os.path.exists(self.model_filename):
                    raise RuntimeError("Model file '%s' not found while reloading in '%s'."
                                       % (self.model_filename, cwd))
                # Parse NPSS model.
                self._top.parseFile(self.model_filename)
                # Add non-NPSS distribution input files to external_files list.
                paths = self._top.inputFileList
                paths.sort()
                for path in paths:
                    if not path or path.startswith(os.environ['NPSS_TOP']):
                        continue
                    if path.startswith(cwd):
                        path = path[len(cwd):]
                    for meta in self.external_files:
                        if meta['path'] == path:
                            break
                    else:
                        self.external_files.append({'path':path, 'input':True})

            if is_reload:
                for name, ref_name, iostat in removed_traits:
                    if not self.trait(name):
                        self.make_public((name, ref_name, iostat))
                # Need to restore input values.
                for name, value in saved_inputs.items():
                    self.set(name, value[0], force=True)
        finally:
            self.pop_dir()

    def create_in_model(self, base_typ, typ, name):
        """ Create object in model. """
        return self._top.create(base_typ, typ, name)

    def get(self, path, index=None):
        """ Return value for attribute. """
        if index is None:
            return super(NPSScomponent, self).get(path, index)
        else:
            self.raise_exception('Indexing not supported yet',
                                 NotImplementedError)
            
    def set(self, path, value, index=None, srcname=None, force=False):
        """ Set attribute value. """
        if index is None:
            super(NPSScomponent, self).set(path, value, index=index,
                                           srcname=srcname, force=force)
        else:
            self.raise_exception('Indexing not supported yet',
                                 NotImplementedError)

    def _check_trait_settable(self, name, srcname=None, force=False):
        trait = self.trait(name)
        if not trait:
            # try to create a trait on-the-fly to map to an NPSS variable
            trait = self._build_trait(name)
            self.add_trait(name, trait)
        
        return super(NPSScomponent, self)._check_trait_settable(name, srcname,
                                                                force)
        
    def execute(self):
        """ Perform operations associated with running the component. """
        if self.reload_model:
            self.info('External reload request.')
            try:
                self.reload()
            except Exception, exc:
                self.raise_exception('Exception during reload: %s' % exc,
                                     RuntimeError)
        elif self.reload_flag:
            try:
                reload_req = getattr(self._top, self.reload_flag)
            except Exception, exc:
                self.raise_exception("Exception getting '%s': %s"
                                     % (self.reload_flag, exc), RuntimeError)
            else:
                if reload_req:
                    self.info('Internal reload request.')
                    try:
                        self.reload()
                    except Exception, exc:
                        self.raise_exception('Exception during reload: %s'
                                             % exc, RuntimeError)
        try:
            if self.run_command:
                self._top.parseString(self.run_command+';')
            else:
                self._top.run()
        except Exception, exc:
            self.raise_exception('Exception during run: %s' % exc,
                                 RuntimeError)

    def parse_string(self, txt):
        return self._top.parseString(txt)
    
    def _build_trait(self, ref_name, iostatus=None, trait=None):
        """
        Do the following on-the-fly rather than having to 
        manually define variables:

        1. Get the correct array type (default is float).
        2. Set the units from translated NPSS units.
        3. Set the doc string from the description attribute.
        4. Create FileTraits for stream objects.
        """
        
        doc = None
        try:
            doc = getattr(self._top, ref_name+'.description')
        except AttributeError:
            pass
                
        if iostatus is None:
            try:
                iostat = _iodict[self._top._get(ref_name+'.IOstatus').lower()]
            except (AttributeError, KeyError):
                self.raise_exception("can't determine iostatus for '%s'" %
                                     ref_name, TraitError)
        else:
            iostat = iostatus
            
        if trait is None:
            try:
                typ = self._top.evalExpr(ref_name+'.getDataType()')
            except RuntimeError:
                metadata = {}
                try:
                    typ = self._top.evalExpr(ref_name+'.isA()')
                except RuntimeError:
                    self.raise_exception("can't determine type of NPSS variable '%s'"
                                         % ref_name, RuntimeError)
                if typ == 'InFileStream':
                    iostat = 'in'
                    typ = 'Stream'
                elif typ == 'OutFileStream':
                    iostat = 'out'
                    typ = 'Stream'
                    metadata['content_type'] = \
                        getattr(self._top, ref_name+'.contentType')
                    metadata['binary'] = \
                        getattr(self._top, ref_name+'.binary') != 0
                    metadata['single_precision'] = \
                        getattr(self._top, ref_name+'.singlePrecision') != 0
                    metadata['unformatted'] = \
                        getattr(self._top, ref_name+'.unformatted') != 0
            
        # Primitive method to create correct type.
            if typ == 'real':
                try:
                    npss_units = getattr(self._top, ref_name+'.units')
                except AttributeError:
                    mdao_units = Undefined
                else:
                    if npss_units:
                        if self.have_units_translation(npss_units):
                            mdao_units = self.get_units_translation(npss_units)
                        else:
                            self.warning("No units translation for '%s'"
                                         % npss_units)
                            mdao_units = Undefined
                    else:
                        mdao_units = Undefined
                if mdao_units is Undefined or mdao_units is None:
                    trait = Float(iostatus=iostat, desc=doc)
                else:
                    trait = UnitsFloat(iostatus=iostat, 
                                      desc=doc, units=mdao_units)
            elif typ == 'int':
                trait = Int(iostatus=iostat, desc=doc)
            elif typ == 'string':
                trait = Str(iostatus=iostat, desc=doc)
            elif typ == 'real[]':
                trait = Array(dtype=numpy.float, shape=(None,), 
                             iostatus=iostat, desc=doc)
            elif typ == 'int[]':
                trait = Array(dtype=numpy.int, shape=(None,), 
                             iostatus=iostat, desc=doc)
            elif typ == 'string[]':
                trait = List(str, iostatus=iostat, desc=doc)
            elif typ == 'real[][]':
                trait = Array(dtype=numpy.float, shape=(None, None),
                             iostatus=iostat, desc=doc)
            elif typ == 'int[][]':
                trait = Array(dtype=numpy.int, shape=(None, None), 
                             iostatus=iostat, desc=doc)
            elif typ == 'real[][][]':
                trait = Array(dtype=numpy.float, shape=(None, None, None),
                             iostatus=iostat, desc=doc)
            elif typ == 'Stream':
                trait = FileTrait(iostatus=iostat, desc=doc, **metadata)
            else:
                self.raise_exception("'%s' is an unsupported NPSS type: '%s'" % 
                                     (ref_name, typ), NotImplementedError)
        
        return NPSSProperty(trait=trait, ref_name=trait.ref_name or ref_name)
    
    @staticmethod
    def have_units_translation(npss_units):
        """ Return True if we can translate npss_units. """
        return units.have_translation(npss_units)

    @staticmethod
    def get_units_translation(npss_units):
        """ Return translation for npss_units. """
        return units.get_translation(npss_units)

    @staticmethod
    def set_units_translation(npss_units, mdao_units):
        """ Set translation for npss_units. """
        return units.set_translation(npss_units, mdao_units)

