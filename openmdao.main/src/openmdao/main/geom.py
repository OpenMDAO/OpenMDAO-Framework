
from openmdao.main.interfaces import IParametricGeometry, implements
from openmdao.main.publisher import publish

class ParametricGeometry(object):
    """A base class for objects implementing the IParametricGeometry
    interface.
    """

    implements(IParametricGeometry)

    def __init__(self):
        self._callbacks = []

    def regen_model(self):
        """Rebuid the geometry based on the current values of the input parameters."""
        raise NotImplementedError("regen_model")

    def list_parameters(self):
        """Return a list of (name, meta) where name is the name of the parameter
        and meta is a metadata dict.
        """
        raise NotImplementedError('list_parameters')

    def set_parameter(self, name, value):
        """Set the value of the named input parameter."""
        raise NotImplementedError("set_parameter")
    
    def get_parameters(self, names):
        """Return a list of values of the named parameters."""
        raise NotImplementedError("get_parameters")
    
    def get_static_geometry(self):
        """Return a 'static' instance of this geometry."""
        raise NotImplementedError("get_static_geometry")
    
    def register_param_list_changedCB(self, callback):
        """Register a callback that will be called when self.invoke_callbacks() is called.
        self.invoke_callbacks() should be called from the inheriting class whenever any
        parameters are added, removed, or change their type.
        """
        self._callbacks.append(callback)

    def invoke_callbacks(self):
        """Invokes any callbacks that have been registered via register_param_list_changedCB."""

        for cb in self._callbacks:
            cb()

    def get_attributes(self, io_only=True):
        """Return an attribute dict for use by the openmdao GUI. You only need to
        override this if you have inputs that user must set directly into your
        ParametricGeometry object.  For example, GEMParametricGeometry has a model_file
        input that the user can set in the GUI to change the .csm file that supplies
        the geometry.
        """

        # the commented out section below shows an example of how you would report 
        # the existence of an input named 'model_file' to the OpenMDAO GUI. 
        return {
            'type': type(self).__name__,
            'Inputs': [
                # {
                #     'name': 'model_file',
                #     'id': 'model_file',
                #     'type': type(self._model_file).__name__,
                #     'value': self._model_file,
                #     'connected': '',
                #}
            ]
        }


class Pub_WV_Wrapper(WV_Wrapper):
    def __init__(self, name):
        super(Pub_WV_Wrapper, self).__init_()
        self.objname = name
        
        # TODO: make this buffer internal to WV_Wrapper
        self.buf = self.get_bufflen()*b'\0'

    def send_geometry(self, first=False):
        self.prepare_for_sends()

        if first:
            self.send_GPrim(self, self.buf,  1, self.send_binary_data)  # send init packet
            self.send_GPrim(self, self.buf, -1, self.send_binary_data)  # send initial suite of GPrims
        else:  #FIXME: add updating of GPRims here...
            pass

        self.wv.finish_sends()
        
    def send_binary_data(self, wsi, buf, ibuf):
        """This is called multiple times during the sending of a 
        set of graphics primitives.
        """
        publish(self.objname, buf, binary=True)
