"""
An NPSS wrapper component.
"""

__all__ = ('NPSScomponent',)
__version__ = '0.1'

import os

from openmdao.main import Component, ArrayVariable, Bool, Dict, Float, \
                          FileVariable, Int, String, StringList
from openmdao.main.component import RUN_OK, RUN_FAILED
from openmdao.main.variable import INPUT, OUTPUT, UNDEFINED

import npss

import units

class NPSScomponent(Component):
    """
    An NPSS wrapper component.  Supports reload requests either internally
    via a flag in the model or externally by setting the reload_model flag.

    TODO: Save external files as part of component state/config.

    TODO: Support index argument to get() and set().

    TODO: Detect & flag execution errors.

    TODO: Save model state as well as component state.

    TODO: Use buffer protocol for array access.

    TODO: Safe multi-thread access (synchronized).

    TODO: CORBA support.
    """

    _dummy = npss.npss()  # Just for context switching.

    @staticmethod
    def grab_context():
        """ Cause a session switch to save correct directory context. """
        NPSScomponent._dummy.exists('COPYRIGHT')

    def __init__(self, name='NPSS', parent=None, doc=None, directory='',
                 arglist=None, output_filename='', top=''):
        super(NPSScomponent, self).__init__(name, parent, doc, directory)
        self._topstr = top

        # Model options.
        String('model_filename', self, INPUT, default='',
               doc='Filename for NPSS model.')

        StringList('include_dirs', self, INPUT, default=[],
                   doc='Model include directories.')

        Bool('use_default_paths', self, INPUT, default=True,
             doc='Use default NPSS directories.')

        Dict('preprocessor_vars', self, INPUT, default={},
             doc='Preprocessor variable definitions')

        # Execution options.
        String('run_command', self, INPUT, default='',
               doc='String to parse to run model.')

        String('reload_flag', self, INPUT, default='',
               doc='Path to flag to internally request a model reload.')

        StringList('preloaded_dlms', self, INPUT, default=[],
                   doc='Preloaded DLMs.')

        Bool('iclod_first', self, INPUT, default=False,
             doc='Search ICLOD before DCLOD.')

        Bool('no_dclod', self, INPUT, default=False,
             doc='Do not search DCLOD.')

        Bool('no_iclod', self, INPUT, default=False,
             doc='Do not search ICLOD.')

        Bool('use_corba', self, INPUT, default=False,
             doc='Enable distributed simulation via CORBA.')

        # Output options.
        String('output_filename', self, INPUT, default=output_filename,
               doc='Filename for standard streams in all new sessions.')

        Bool('trace_execution', self, INPUT, default=False,
             doc='Trace interpreted statement execution.')

        # Advanced options.
        String('assembly_type', self, INPUT, default='',
               doc='Type for top object.')

        String('executive_type', self, INPUT, default='',
               doc='Top-level executive.')

        StringList('preloaded_objs', self, INPUT, default=[],
                   doc='Preloaded Objects.')

        Bool('use_solver', self, INPUT, default=True,
             doc='Use default solver.')

        Bool('use_constants', self, INPUT, default=True,
             doc='Use default constants.')

        String('access', self, INPUT, default='',
               doc='Default access type.')

        Bool('autodoc', self, INPUT, default=False,
             doc='Allow abstract creation.')

        String('ns_ior', self, INPUT, default='',
               doc='IOR of NamingService.')

        String('other_opts', self, INPUT, default='',
               doc='Other options.')

        # Wrapper stuff.
        Bool('reload_model', self, INPUT, default=False,
             doc='Flag to externally request a model reload.')

        if arglist is not None:
            self._parse_arglist(arglist)
        self._top = None
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
        if super(NPSScomponent, self).post_load():
            try:
                self.reload()
                return True
            except Exception, exc:
                self.error('Reload caught exception: %s', str(exc))
        return False

    def pre_delete(self):
        """ Perform any required operations before the model is deleted. """
        if self._top is not None:
            self._top.closeSession()
            self._top = None
        super(NPSScomponent, self).pre_delete()

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

        for arg in arglist:
            if arg == '-C':
                obj_next = True
            elif obj_next:
                self.preloaded_objs.append(arg)
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
                self.include_dirs.append(arg)
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
                self.preloaded_dlms.append(arg)
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
                self.ns_ior = arg
                ns_next = False
            elif arg == '-trace':
                self.trace_execution = True
            elif arg.startswith('-'):
                self.raise_exception("illegal option '%s'" % arg, ValueError)
            else:
                self.model_filename = arg

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
        cwd = os.getcwd()+os.sep
        is_reload = False
        saved_inputs = []
        if self._top is not None:
            is_reload = True
            # Save current input values.
            if self.parent is not None:
                connections = self.parent.list_connections()
                for src, dst in connections:
                    dst_comp, dst_attr = dst.split('.', 1)
                    if dst_comp == self.name:
                        saved_inputs.append((dst_attr, self.get(dst_attr)))
            # Remove model input files from external_files list.
            paths = self._top.inputFileList
            for path in paths:
                if path.startswith(cwd):
                    path = path[len(cwd):]
                for i, meta in enumerate(self.external_files):
                    if meta['path'] == path:
                        self.external_files.pop(i)
                        break
            self._top.closeSession()
            self._top = None

        # Default session directory is set during initialization.
        directory = self.get_directory()
        pushed = False
        try:
            self.push_dir(directory)
            pushed = True
        except OSError, exc:
            self.error("Could not move to execution directory '%s': %s",
                       directory, exc.strerror)

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
            # Need to restore input values.
            for name, value in saved_inputs:
                self.set(name, value)

        self.grab_context()
        if pushed:
            self.pop_dir()

    def create_in_model(self, base_typ, typ, name):
        """ Create object in model. """
        return self._top.create(base_typ, typ, name)

    def get(self, path, index=None):
        """ Return value for attribute. """
        if index is None:
            return getattr(self, path)
        else:
            self.raise_exception('Indexing not supported yet',
                                 NotImplementedError)

    def __getattr__(self, name):
        """
        Return value for attribute.
        Note that this is not __getattribute__.
        This gets called only when the normal methods fail.
        """
        if name == '_top':
            return object.__getattribute__(self, name)

        try:
            top = object.__getattribute__(self, '_top')
        except AttributeError:
            return super(NPSScomponent, self).__getattribute__(name)
        if top is None:
            return super(NPSScomponent, self).__getattribute__(name)

        try:
            return getattr(top, name)
        except AttributeError, err:
            # Possibly a wrapper attribute.
            try:
                return super(NPSScomponent, self).__getattribute__(name)
            except AttributeError:
                raise AttributeError(self.get_pathname()+' '+str(err))
        finally:
            self.grab_context()

    def set(self, path, value, index=None):
        """ Set attribute value. """
        if index is None:
            setattr(self, path, value)
        else:
            self.raise_exception('Indexing not supported yet',
                                 NotImplementedError)

    def __setattr__(self, name, value):
        """ Set attribute value. """
        if name == '_top':
            return object.__setattr__(self, name, value)

        try:
            top = object.__getattribute__(self, '_top')
        except AttributeError:
            return super(NPSScomponent, self).__setattr__(name, value)
        if top is None:
            return super(NPSScomponent, self).__setattr__(name, value)

        try:
            setattr(top, name, value)
        except AttributeError:
            return super(NPSScomponent, self).__setattr__(name, value)
        finally:
            self.grab_context()

    def execute(self):
        """ Perform operations associated with running the component. """
        status = RUN_OK

        if self.reload_model:
            self.info('External reload request.')
            try:
                self.reload()
            except Exception, exc:
                self.error('Exception during reload: %s', str(exc))
                status = RUN_FAILED
        else:
            if self.reload_flag:
                try:
                    reload_req = getattr(self._top, self.reload_flag)
                except Exception, exc:
                    self.error('Exception getting %s: %s',
                               self.reload_flag, str(exc))
                    status = RUN_FAILED
                else:
                    if reload_req:
                        self.info('Internal reload request.')
                        try:
                            self.reload()
                        except Exception, exc:
                            self.error('Exception during reload: %s', str(exc))
                            status = RUN_FAILED

        if status == RUN_OK:
            try:
                if self.run_command:
                    self._top.parseString(self.run_command+';')
                else:
                    self._top.run()
            except Exception, exc:
                self.error('Exception during run: %s', str(exc))
                status = RUN_FAILED

        self.grab_context()
        return status

    def make_public(self, obj_info):
        """
        Overloading make_public() so that we can do the following
        on-the-fly rather than having to manually define variables:

        1. Get the correct array type (default is float).
        2. Set the units from translated NPSS units.
        3. Set the doc string from the description attribute.
        4. Create FileVariables for stream objects.
        """
        if isinstance(obj_info, list):
            lst = obj_info
        else:
            lst = [obj_info]

        new_info = []
        for entry in lst:
            iostat = INPUT
            metadata = {}

            if isinstance(entry, basestring):
                name = entry
            elif isinstance(entry, tuple):
                name = entry[0]  # wrapper name
                if len(entry) > 2:
                    iostat = entry[2] # optional iostatus
            else:
                new_info.append(entry)
                continue

            try:
                typ = self.evalExpr(name+'.getDataType()')
            except RuntimeError:
                try:
                    typ = self.evalExpr(name+'.isA()')
                except RuntimeError:
                    new_info.append(entry)
                    continue
                else:
                    if typ == 'InFileStream':
                        typ = 'Stream'
                        iostat = INPUT
                    elif typ == 'OutFileStream':
                        typ = 'Stream'
                        iostat = OUTPUT
                        metadata['content_type'] = \
                            getattr(self, name+'.contentType')
                        metadata['binary'] = \
                            getattr(self, name+'.binary') != 0
                        metadata['single_precision'] = \
                            getattr(self, name+'.singlePrecision') != 0
                        metadata['unformatted'] = \
                            getattr(self, name+'.unformatted') != 0
                    else:
                        new_info.append(entry)
                        continue

            try:
                npss_units = getattr(self, name+'.units')
            except AttributeError:
                mdao_units = UNDEFINED
            else:
                if npss_units:
                    if self.have_units_translation(npss_units):
                        mdao_units = self.get_units_translation(npss_units)
                    else:
                        self.warning("No units translation for '%s'" % npss_units)
                        mdao_units = UNDEFINED
                else:
                    mdao_units = UNDEFINED

            try:
                doc = getattr(self, name+'.description')
            except AttributeError:
                doc = None
            else:
                if not doc:
                    doc = None

            # Primitive method to create correct type.
            if typ == 'real':
                dobj = Float(name, self, iostat, doc=doc, units=mdao_units)
            elif typ == 'int':
                dobj = Int(name, self, iostat, doc=doc)
            elif typ == 'string':
                dobj = String(name, self, iostat, doc=doc)
            elif typ == 'real[]':
                dobj = ArrayVariable(name, self, iostat, float, doc=doc,
                                     num_dims=1)
            elif typ == 'int[]':
                dobj = ArrayVariable(name, self, iostat, int, doc=doc,
                                     num_dims=1)
            elif typ == 'string[]':
                dobj = StringList(name, self, iostat, doc=doc)
            elif typ == 'real[][]':
                dobj = ArrayVariable(name, self, iostat, float, doc=doc,
                                     num_dims=2)
            elif typ == 'int[][]':
                dobj = ArrayVariable(name, self, iostat, int, doc=doc,
                                     num_dims=2)
            elif typ == 'real[][][]':
                dobj = ArrayVariable(name, self, iostat, float, doc=doc,
                                     num_dims=3)
            elif typ == 'Stream':
                dobj = FileVariable(name, self, iostat, doc=doc,
                                    ref_name=name+'.filename',
                                    metadata=metadata)
            else:
                self.raise_exception('Unsupported NPSS type: %s' % typ,
                                     NotImplementedError)

            new_info.append(dobj)

        return super(NPSScomponent, self).make_public(new_info)

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

