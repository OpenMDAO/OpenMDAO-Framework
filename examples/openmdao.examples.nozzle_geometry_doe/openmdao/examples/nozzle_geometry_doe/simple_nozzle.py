import os
import numpy as np

import openmdao.lib.geometry.stl as stl
from openmdao.lib.geometry.ffd_axisymetric import Body, Shell
from openmdao.lib.geometry.stl_group import STLGroup


class PlugNozzleGeometry(STLGroup): 

    def __init__(self): 
        super(PlugNozzleGeometry,self).__init__()

        this_dir, this_filename = os.path.split(os.path.abspath(__file__))
        plug_file = os.path.join(this_dir, 'plug.stl')
        plug = stl.STL(plug_file)
        cowl_file = os.path.join(this_dir, 'cowl.stl')
        cowl = stl.STL(cowl_file)
        
        n_c = 10
        body = Body(plug,controls=n_c) #just makes n_C evenly spaced points
        shell = Shell(cowl,cowl.copy(),n_c,n_c)

        self.add(body,name="plug")
        self.add(shell,name="cowl")

    def get_visualization_data(self, wv):
        super(PlugNozzleGeometry, self).get_visualization_data(wv)

if __name__=="__main__": 
    pn = PlugNozzleGeometry()
    pn._provideJ()

    arg = dict([(p[0],p[1]['value']) for p in pn.list_parameters()])
    result = {'geom_out':np.zeros((len(pn.points),3))}

    print pn.apply_deriv(arg, result)

    print pn.apply_derivT(result, arg)






