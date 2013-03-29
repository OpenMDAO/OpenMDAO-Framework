
from openmdao.main.component import Component
from openmdao.main.container import Container
from openmdao.main.interfaces import IParametricGeometry, IStaticGeometry
from openmdao.main.datatypes.api import Slot, Geom
from openmdao.util.log import logger
from openmdao.main.datatypes.api import Float, Int, Str, Python, List

_ttdict = {
    float: Float,
    int: Int,
    long: Int,
    str: Str,
    unicode: Str,
    list: List,
}

class GeomComponent(Component):

    # pylint: disable-msg=E1101
    parametric_geometry = Slot(IParametricGeometry, allow_none=True,
                               desc='Slot for a parametric geometry.')

    geometry_output = Geom(IStaticGeometry, iotype='out',
                           desc ='Geometry object')
    
    def __init__(self):
        super(GeomComponent, self).__init__()
        self._class_names = set(self.traits().keys())
        self._input_var_names = set()
        self._output_var_names = set()

    def _parametric_geometry_changed(self, old, new):
        """Called whenever the parametric geometry is set.
        """
        self._update_iovar_set()
        
        if new is not None:
            if isinstance(new, Container):
                new.parent = self
                new.name = 'parametric_geometry'
            new.register_param_list_changedCB(self._model_updated)
            
            self.geometry_output = new.get_static_geometry()
        else:
            self.geometry_output = None

    def _model_updated(self):
        """Should be called by the parametric_geometry object whenever
        the list of parameters changes, so that we can update our list
        of inputs and outputs.
        """
        self._update_iovar_set()
        self._update_comp_outputs()

    def execute(self):
        """Rebuild the geometry using the current set of parameters.
        """
        if self.parametric_geometry is not None:
            self.parametric_geometry.regen_model()
            self._update_comp_outputs()

    def _update_comp_outputs(self):
        """Set the values of the component outputs based on their
        corresponding values in the geometry.
        """
        if self._output_var_names:
            outs = self.parametric_geometry.get_parameters(self._output_var_names)
            for name,out in zip(self._output_var_names, outs):
                setattr(self, name, out)

    def _update_iovar_set(self):
        """Determine the set of input and output variables for the
        current parametric geometry.
        """
        old_in = self._input_var_names
        old_out = self._output_var_names
            
        inps, outps = self._get_io_info()

        self._input_var_names = set([p[0] for p in inps])
        self._output_var_names = set([p[0] for p in outps])
        
        added_ins = self._input_var_names - old_in
        added_outs = self._output_var_names - old_out
        
        removed_ins = old_in - self._input_var_names
        removed_outs = old_out - self._output_var_names
        
        for name in removed_ins:
            self._remove_var(name)
        for name in removed_outs:
            self._remove_var(name)

        for plist in (inps, outps):
            for name, meta in plist:
                if name in added_ins or name in added_outs:
                    val = meta['value']
                    typ = _ttdict.get(type(val))
                    del meta['value'] # don't include value in trait metadata
                    if typ is None:
                        typ = Python   # FIXME
                    self.add_trait(name, typ(val, **meta))
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
        if self.parametric_geometry:
            paraminfos = self.parametric_geometry.list_parameters()
            cnames = self._class_names
            inter = []
            params = []
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
                                   % (p[0],p[1]['iotype']))
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

    def _input_updated(self, name):
        if self.parametric_geometry is not None and name in self._input_var_names:
            self.parametric_geometry.set_parameter(name, getattr(self, name))
        super(GeomComponent, self)._input_updated(name)

