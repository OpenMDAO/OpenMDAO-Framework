from openmdao.main.component import Component
from openmdao.main.container import Container
from openmdao.main.vartree import VariableTree
from openmdao.main.interfaces import IParametricGeometry, IStaticGeometry
from openmdao.main.datatypes.api import Slot, Geom, Array, Enum, VarTree
from openmdao.main.datatypes.api import Float, Int, Str, Python, List, Dict, Bool
from openmdao.util.log import logger

_ttdict = {
    float: Float,
    int: Int,
    long: Int,
    str: Str,
    unicode: Str,
    list: List,
    bool: Bool,
    dict: Dict,
    'enum': Enum,
    'float': Float,
    'int': Int,
    'str': Str,
    'list': List,
    'dict': Dict,
    'bool': Bool,
}

try:
    import numpy
except ImportError:
    pass
else:
    _ttdict[numpy.ndarray] = Array
    _ttdict['array'] = Array

def _get_trait_from_meta(name, meta):
    """Create a Variable object based on the contents
    of meta, which contains a 'value', plus possibly
    other information, e.g., 'type'.
    """
    meta = meta.copy()
    val = meta['value']
    
    try:
        # if 'type' is provided in the metadata, use that
        if 'type' in meta:
            typ = _ttdict[meta['type']]
        else:  # otherwise just infer the Variable type from the value type
            typ = _ttdict[type(val)]
    except KeyError:
        if isinstance(val, list):
            typ = List
        elif isinstance(val, dict):
            typ = Dict
        else:
            logger.warning("no Variable type found for key of type %s (value=%s), using Python Variable type, which performs no validation" % (type(val),val))
            typ = Python   # FIXME
            
    del meta['value']  # don't include value in trait metadata
    return typ(val, **meta)

def _create_trait(parent, name, meta):
    """Create a trait based on the type of value and
    the contents of name.  If name contains dots, create
    VarTrees to represent non-leaf nodes of the tree.
    """
    if '.' in name:
        iotype = meta.get('iotype')
        # see if the necessary VarTree parents exist yet
        parts = name.split('.')
        for part in parts[:-1]:
            if not hasattr(parent, part):
                parent.add(part, VarTree(VariableTree(), iotype=iotype))
            parent = getattr(parent, part)
        parent.add(parts[-1], _get_trait_from_meta(name, meta))
    else:  # just a simple variable
        parent.add(name, _get_trait_from_meta(name, meta))


class GeomComponent(Component):

    # pylint: disable-msg=E1101
    parametric_geometry = Slot(IParametricGeometry, allow_none=True,
                               desc='Slot for a parametric geometry.')

    geom_out = Geom(IStaticGeometry, iotype='out',
                  desc='a geometry generated using the set of current input parameters')

    auto_run = Bool(False, iotype="in", desc="When set to True, component will automatically execute whenever any input values are changed")

    def __init__(self):
        super(GeomComponent, self).__init__()
        self._class_names = set(self.traits().keys())
        self._input_var_names = set()
        self._output_var_names = set()

        self.on_trait_change(self._auto_run_notify,'auto_run')
        #self.on_trait_change(self._test_notify,'auto_run')

    

    def _parametric_geometry_changed(self, old, new):
        """Called whenever the parametric geometry is set.
        """
        self._update_iovar_set()

        if new is not None:
            if isinstance(new, Container):
                new.parent = self
                new.name = 'parametric_geometry'
            new.register_param_list_changedCB(self._model_updated)
            try:
                self.geom_out = self.parametric_geometry.get_static_geometry()
            except:
                self.geom_out = None
        else:
            self.geom_out = None

    def _model_updated(self):
        """Should be called by the parametric_geometry object whenever
        the list of parameters changes so that we can update our list
        of inputs and outputs.
        """
        self._update_iovar_set()
        self._update_comp_outputs()

    def execute(self):
        """Rebuild the geometry using the current set of parameters.
        """
        if self.parametric_geometry is not None:
            try:
                self.parametric_geometry.regen_model()
            except Exception as err:
                logger.error("ERROR:"+str(err))
                raise
            self._update_comp_outputs()

    def _update_comp_outputs(self):
        """Set the values of the component outputs based on their
        corresponding values in the geometry.
        """
        if self._output_var_names:
            outs = self.parametric_geometry.get_parameters(self._output_var_names)
            for name, out in zip(self._output_var_names, outs):
                setattr(self, name, out)
        self.geom_out = self.parametric_geometry.get_static_geometry()

    def _auto_run_notify(self,new=False): 
        """Adds or removes callbacks from variable changes to call run. New will
        be value of auto_run variable.""" 
        for var in self._input_var_names: 
            self.on_trait_change(self.run,name=var,remove=(not new))


    def _var_cleanup(self, names):
        for name in names:
            if '.' not in name:
                self._remove_var(name)

    def _update_iovar_set(self):
        """Determine the set of input and output variables for the
        current parametric geometry and create Variable objects
        at the component level.
        """
        old_in = self._input_var_names
        old_out = self._output_var_names

        #clear all the callbacks
        self._auto_run_notify(False)        

        inps, outps = self._get_io_info()

        # these are flattened lists of names, so they 
        # may contain dots
        self._input_var_names = set([p[0] for p in inps])
        self._output_var_names = set([p[0] for p in outps])

        #if active, turn on auto-run
        if self.auto_run: 
            self._auto_run_notify(True)

        added_ins = self._input_var_names - old_in
        added_outs = self._output_var_names - old_out

        removed_ins = old_in - self._input_var_names
        removed_outs = old_out - self._output_var_names

        # get names of any vartrees that have been removed from or added to
        vtnames = set([p.split('.')[0] for p in 
                      added_ins|added_outs|removed_ins|removed_outs 
                      if '.' in p])

        # clean up modified vartrees
        self._var_cleanup(vtnames)

        # now cleanup any regular (non-vartree) variables
        self._var_cleanup(removed_ins)
        self._var_cleanup(removed_outs)

        for plist in (inps, outps):
            for name, meta in plist:
                if name in added_ins or name in added_outs or '.' in name:
                    val = meta['value']
                    _create_trait(self, name, meta)
                    if meta['iotype'] == 'in':
                        setattr(self, name, val)

    def _remove_var(self, name):
        """Removes the specified variable."""
        if self.parent:
            self.parent.disconnect('.'.join([self.name, name]))
        self.remove_trait(name)

    def _get_io_info(self):
        """Returns a tuple of (inputs, outputs) where inputs and outputs are
        lists of tuples of the form (name, meta) for each parameter.
        """
        params = []
        if self.parametric_geometry:
            paraminfos = self.parametric_geometry.list_parameters()
            cnames = self._class_names
            inter = []
            for p in paraminfos:
                if p[0] in cnames:
                    inter.append(p[0])
                elif self._eligible(p[0]):
                    params.append(p)
            if inter:
                logger.warning("the following variables already exist in "
                               "GeomComponent and will be ignored: %s" % inter)

        ins = []
        outs = []
        for p in params:
            try:
                io = p[1]['iotype']
            except KeyError:
                raise RuntimeError("parameter %s has no iotype metadata" % p[0])
            if io == 'in':
                ins.append(p)
            elif io == 'out':
                outs.append(p)
            else:
                raise RuntimeError("parameter %s does not have valid iotype metadata (iotype='%s'), must be 'in' or 'out'"
                                   % (p[0], p[1]['iotype']))
        return (ins, outs)

    def _eligible(self, name):
        """Return True if the named trait is not excluded from the public interface
        based on the includes and excludes lists.
        """
        if name in self._class_names:
            return False
        # if self.includes and name not in self.includes:
        #     return False
        # elif self.excludes and name in self.excludes:
        #     return False
        return True

    def _input_updated(self, name, fullpath=None):
        if fullpath is None:
            attr = getattr(self, name)
        else:
            name = fullpath
            attr = self
            for part in fullpath.split('.'):
                attr = getattr(attr, part)
        if self.parametric_geometry is not None and name in self._input_var_names:
            self.parametric_geometry.set_parameter(name, attr)
        super(GeomComponent, self)._input_updated(name.split('.',1)[0])

    def _set_failed(self, path, value, index=None, src=None, force=False):
        # check to see if dest attribute is inside of our parametric_geometry
        # object
        obj = self
        try:
            parts = path.split('.')
            if len(parts) > 1:
                for name in parts[:-1]:
                    obj = getattr(obj, name)
            if index is None:
                setattr(obj, parts[-1], value)
            else:
                raise RuntimeError('index not supported')
        except AttributeError as err:
            super(GeomComponent, self)._set_failed(path, value, index, src, force)

