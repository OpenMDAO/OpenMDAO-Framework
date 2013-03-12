
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
        self._input_var_names = None
        self._output_var_names = None

    def _parametric_geometry_changed(self, old, new):
        """Called whenever the parametric geometry is set.
        """
        self._update_iovar_set()
        
        if new is not None:
            if isinstance(new, Container):
                new.parent = self
                new.name = 'parametric_geometry'
            new.register_param_list_changedCB(self._model_updated)
            
        self.geometry_output = new.get_geometry()

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
            self.parametric_geometry.regenModel()
            self._update_comp_outputs()

    def _update_comp_outputs(self):
        """Set the values of the component outputs based on their
        corresponding values in the geometry.
        """
        if self._output_var_names is not None:
            for name in self._output_var_names:
                out = self.parametric_geometry.getParameter(name)['value']
                setattr(self, name, out)

    def _update_iovar_set(self):
        """Determine the set of input and output variables for the
        current parametric geometry.
        """
        old_in = set()
        if self._input_var_names is not None:
            old_in.update(self._input_var_names)
        old_out = set()
        if self._output_var_names is not None:
            old_out.update(self._output_var_names)
            
        self._input_var_names = None
        self._output_var_names = None
        
        new_in = set(self.input_var_names())
        new_out = set(self.output_var_names())
        
        added_outs = new_out - old_out
        added_ins = new_in - old_in
        
        removed_outs = old_out - new_out
        removed_ins = old_in - new_in
        
        for name in removed_ins:
            self._remove_var(name)
        for name in added_ins:
            self._add_input(name)
            
        for name in removed_outs:
            self._remove_var(name)
        for name in added_outs:
            self._add_output(name)

    def _add_input(self, name):
        """Adds the specified input variable."""
        param = self.parametric_geometry.getParameter(name)
        val = param['value']
        typ = _ttdict.get(type(val))
        if typ is None:
            typ = Python   # FIXME
        self.add_trait(name, typ(val, iotype='in'))
        setattr(self, name, val)
    
    def _add_output(self, name):
        """Adds the specified output variable."""
        val = self.parametric_geometry.getParameter(name)
        typ = _ttdict.get(type(val))
        if typ is None:
            typ = Python   # FIXME
        self.add_trait(name, typ(val, iotype='out'))
    
    def _remove_var(self, name):
        """Removes the specified variable."""
        if self.parent:
            self.parent.disconnect('.'.join([self.name, name]))
        self.remove_trait(name)

    def _update_io_names(self):
        if self.parametric_geometry:
            params = self.parametric_geometry.listParameters()
            inter = self._class_names.intersection([p[0] for p in params])
            if inter:
                logger.warning("the following variables already exist in "
                               "GeomComponent and will be ignored: %s" % 
                               list(inter))
            params = [p for p in params if self._eligible(p[0]) and 
                                       p[0] not in self._class_names]
            self._input_var_names = set([p[0] for p in params
                                         if p[1]['iotype']=='in'])
            self._output_var_names = set([p[0] for p in params
                                         if p[1]['iotype']=='out'])
        else:
            self._input_var_names = set()
            self._output_var_names = set()

    def input_var_names(self):
        """Return the list of names of public inputs that correspond
        to model inputs.
        """
        if self._input_var_names is None:
            self._update_io_names()
        return self._input_var_names

    def output_var_names(self):
        """Return the list of names of public outputs that correspond
        to model outputs.
        """
        if self._output_var_names is None:
            self._update_io_names()
        return self._output_var_names

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
        if self.parametric_geometry is not None and name in self.input_var_names():
            self.parametric_geometry.setParameter(name, getattr(self, name))
        super(GeomComponent, self)._input_updated(name)

