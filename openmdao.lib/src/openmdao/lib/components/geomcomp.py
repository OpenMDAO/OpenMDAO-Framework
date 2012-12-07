
from openmdao.main.component import Component
from openmdao.main.interfaces import IParametricGeometry
from openmdao.main.datatypes.api import Slot


class GeomComponent(Component):

    # pylint: disable-msg=E1101
    geom_builder = Slot(IParametricGeometry, allow_none=True,
                   desc='Slot for a parametric geometry.')

    def __init__(self):
        super(GeomComponent, self).__init__()

    def execute(self):
        """Rebuild the geometry using the current set of parameters.
        """
        pass

    def _model_changed(self, oldmodel, newmodel):
        pass
