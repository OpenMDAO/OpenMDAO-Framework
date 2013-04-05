
from pkg_resources import working_set

from pyV3D._pyV3D import WV_Wrapper, WV_ON, WV_SHADING, WV_ORIENTATION
from openmdao.main.publisher import publish
from openmdao.main.interfaces import IStaticGeometry
from openmdao.main.mp_support import has_interface
from openmdao.util.log import logger

NAME_SIZE = 512

class ObjectViewer(object):
    """This is a base class for custom viewers of OpenMDAO objects."""
    def __init__(self, name, obj):
        self.paddedname = bytes(name)+(NAME_SIZE-len(name))*b'\0'  # 0 padded object name in bytes
        self.obj = obj
    

class WV_ObjectViewer(ObjectViewer):
    """An ObjectViewer that communicates with WebViewer clients to
    allow viewing of 3D geometry.
    """
    def __init__(self, name, obj):
        super(WV_ObjectViewer, self).__init__(name, obj)
        if not has_interface(obj, IStaticGeometry):
            raise RuntimeError("WV_ObjectViewer requires an object with the IStaticGeometry interface")
        
        self.wv = WV_Wrapper()
        # create a storage buffer for the graphics primitive data
        
        # TODO: see if we can use pre-padding of the buffer to allow adding a simple routing
        # protocol without copying the buffer.
        #  Our simple routing protocol is:
        #   - a 0 padded routing string of size 512 bytes
        #   - followed by the actual binary message that will go the webviewer

        self.buf = self.wv.get_bufflen()*b'\0'

        # send the initial version of the geometry
        self.create_geom()
        self.send_geometry(first=True)

    def send_binary_data(self, wsi, buf, ibuf):
        try:
            publish(self.objname, self.nullname+buf)
        except Exception as err:
            logger.error("Exception in send_binary_data:", err)
            return -1
        return 0

    def send_geometry(self, first=False):
        self.wv.prepare_for_sends()

        if first:
            self.wv.send_GPrim(self, self.buf,  1, self.send_binary_data)  # send init packet
            self.wv.send_GPrim(self, self.buf, -1, self.send_binary_data)  # send initial suite of GPrims
        else:  #FIXME: add updating of GPRims here...
            pass

        self.wv.finish_sends()

