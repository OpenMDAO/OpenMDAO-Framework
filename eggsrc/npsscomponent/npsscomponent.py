"""
An NPSS wrapper component
"""

__all__ = ('NPSScomponent',)
__version__ = '0.1'

import os

from openmdao.main.bool import Bool
from openmdao.main.component import Component, RUN_OK, RUN_FAILED
from openmdao.main.dict import Dict
from openmdao.main.string import String
from openmdao.main.stringlist import StringList
from openmdao.main.variable import INPUT

import npss

class NPSScomponent(Component):
    """
    An NPSS wrapper component.
    """

    _dummy = npss.npss()

    @staticmethod
    def grab_context():
        """ Cause a session switch to save correct directory context. """
        NPSScomponent._dummy.exists('COPYRIGHT')

    def __init__(self, name='NPSS', parent=None, desc=None, directory='',
                 arglist=None, output_filename='', top=''):
        super(NPSScomponent, self).__init__(name, parent, desc, directory)
        self._topstr = top

        # Model options.
        String('model_filename', self,
               desc='Filename for NPSS model.',
               default='', iostatus=INPUT)

        StringList('include_dirs', self,
                   desc='Model include directories.',
                   default=[], iostatus=INPUT)

        Bool('use_default_paths', self,
             desc='Use default NPSS directories.',
             default=True, iostatus=INPUT)

        Dict('preprocessor_vars', self,
             desc='Preprocessor variable definitions',
             default={}, iostatus=INPUT)

        # Execution options.
        String('run_command', self,
               desc='String to parse to run model.',
               default='', iostatus=INPUT)

        String('reload_flag', self,
               desc='Path to flag to internally request a model reload.',
               default='', iostatus=INPUT)

        StringList('preloaded_dlms', self,
                   desc='Preloaded DLMs.',
                   default=[], iostatus=INPUT)

        Bool('iclod_first', self,
             desc='Search ICLOD before DCLOD.',
             default=False, iostatus=INPUT)

        Bool('no_dclod', self,
             desc='Do not search DCLOD.',
             default=False, iostatus=INPUT)

        Bool('no_iclod', self,
             desc='Do not search ICLOD.',
             default=False, iostatus=INPUT)

        Bool('use_corba', self,
             desc='Enable distributed simulation via CORBA.',
             default=False, iostatus=INPUT)

        # Output options.
        String('output_filename', self,
               desc='Filename for standard streams in all new sessions.',
               default=output_filename, iostatus=INPUT)

        Bool('trace_execution', self,
             desc='Trace interpreted statement execution.',
             default=False, iostatus=INPUT)

        # Advanced options.
        String('assembly_type', self,
               desc='Type for top object.',
               default='', iostatus=INPUT)

        String('executive_type', self,
               desc='Top-level executive.',
               default='', iostatus=INPUT)

        StringList('preloaded_objs', self,
                   desc='Preloaded Objects.',
                   default=[], iostatus=INPUT)

        Bool('use_solver', self,
             desc='Use default solver.',
             default=True, iostatus=INPUT)

        Bool('use_constants', self,
             desc='Use default constants.',
             default=True, iostatus=INPUT)

        String('access', self,
               desc='Default access type.',
               default='', iostatus=INPUT)

        Bool('autodoc', self,
             desc='Allow abstract creation.',
             default=False, iostatus=INPUT)

        String('ns_ior', self,
               desc='IOR of NamingService.',
               default='', iostatus=INPUT)

        String('other_opts', self,
               desc='Other options.',
               default='', iostatus=INPUT)

        # Wrapper stuff.
        Bool('reload_model', self,
             desc='Flag to externally request a model reload.',
             default=False, iostatus=INPUT)

        if arglist is not None:
            self._parse_arglist(arglist)
        self._top = None
        self._reload_model()

    def __getstate__(self):
        """ Return dict representing this Component's state. """
# TODO: save NPSS model state as well as component state.
        state = super(NPSScomponent, self).__getstate__()
        state['_top'] = None  # pyNPSS is unpickleable.
        return state

    def __setstate__(self, state):
        """ Restore this Component's state. """
# TODO: restore NPSS model state as well as component state.
        super(NPSScomponent, self).__setstate__(state)
        # _top will be set during post_load via _reload_model().

    def post_load(self):
        """ Perform any required operations after model has been loaded. """
        if super(NPSScomponent, self).post_load():
            try:
                self._reload_model()
                return True
            except Exception, exc:
                self.log_error('Reload caught exception: %s', str(exc))
        return False

    def pre_delete(self):
        """ Perform any required operations before the model is deleted. """
        if self._top is not None:
            self._top.closeSession()
            self._top = None
        super(NPSScomponent, self).pre_delete()

    def _parse_arglist(self, arglist):
        """ Parse argument list. """
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
                                         RuntimeError)
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
                self.raise_exception("illegal option '%s'" % arg, RuntimeError)
            else:
                self.model_filename = arg

        if access_next:
            self.raise_exception("expected default access type", RuntimeError)
        elif assembly_next:
            self.raise_exception("expected assembly type", RuntimeError)
        elif dlm_next:
            self.raise_exception("expected DLM path", RuntimeError)
        elif executive_next:
            self.raise_exception("expected executive type", RuntimeError)
        elif include_next:
            self.raise_exception("expected include path", RuntimeError)
        elif ns_next:
            self.raise_exception("expected NameServer IOR path", RuntimeError)
        elif obj_next:
            self.raise_exception("expected object path", RuntimeError)
        elif preproc_next:
            self.raise_exception("expected preprocessor value", RuntimeError)

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

    def _reload_model(self):
        """ (Re)load model. """
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
            self.info('Reloading session in %s', os.getcwd())
        arglist = self._generate_arglist()
        if self.output_filename:
            if not '-singleStream' in arglist:
                arglist.insert(0, '-singleStream')

        self._top = npss.npss(arglist=arglist, top=self._topstr)

        if self.output_filename:
            self._top.cout.append = is_reload
            self._top.cout.filename = self.output_filename
            if is_reload:
                msg = '\nReloading session in '+os.getcwd()+'\n'
                if self.model_filename:
                    msg += 'Model filename '+self.model_filename+'\n'
                self._top.cout.println(msg)
            self.info('output routed to %s', self.output_filename)

        if self.model_filename:
            self._top.parseFile(self.model_filename)
            cwd = os.getcwd()+'/'
            paths = self._top.inputFileList
            paths.sort()
            for path in paths:
                if path.startswith(cwd):
                    path = path[len(cwd):]
                    if path not in self.external_files:
                        self.external_files.append(path)

        if is_reload:
            # Need to restore input values.
            for name, value in saved_inputs:
                self.set(name, value)

        NPSScomponent.grab_context()
        if pushed:
            self.pop_dir()

    def create_in_model(self, base_typ, typ, name):
        """ Create object in model. """
        return self._top.create(base_typ, typ, name)

    def get(self, path, index=None):
        """ Return value for attribute. """
        return getattr(self, path)

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
            NPSScomponent.grab_context()

    def set(self, path, value, index=None):
        """ Set attribute value. """
        setattr(self, path, value)

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
            NPSScomponent.grab_context()

    def execute(self):
        """ Perform operations associated with running the component. """
        status = RUN_OK

        if self.reload_model:
            self.info('External reload request.')
            try:
                self._reload_model()
            except Exception, exc:
                self.error('Exception during reload: %s', exc)
                status = RUN_FAILED
        else:
            if self.reload_flag:
                try:
                    reload_req = getattr(self._top, self.reload_flag)
                except Exception, exc:
                    self.error('Exception getting %s: %s',
                               self.reload_flag, exc)
                    status = RUN_FAILED
                else:
                    if reload_req:
                        self.info('Internal reload request.')
                        try:
                            self._reload_model()
                        except Exception, exc:
                            self.error('Exception during reload: %s', exc)
                            status = RUN_FAILED

        if status == RUN_OK:
            try:
                if self.run_command:
                    self._top.parseString(self.run_command+';')
                else:
                    self._top.run()
            except Exception, exc:
                self.error('Exception during run: %s', exc)
                status = RUN_FAILED

        NPSScomponent.grab_context()
        return status

