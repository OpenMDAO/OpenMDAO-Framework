import struct
import string

import numpy as np
from scipy.sparse import csr_matrix

from stl import ASCII_FACET, BINARY_HEADER, BINARY_FACET

from ffd_axisymetric import Body, Shell

from pyV3D.stl import STLSender
from openmdao.main.interfaces import IParametricGeometry, implements, IStaticGeometry

class STLGroup(object): 


    implements(IParametricGeometry, IStaticGeometry)

    def __init__(self): 

        self._comps = []
        self._i_comps = {}
        self._n_comps = 0 

        #used to store set values of parameters from IParametricGeometry
        self.param_vals = {}
        self.param_name_map = {}

        self._callbacks = []

        self._needs_linerize = True

    def add(self, comp ,name=None): 
        """ addes a new component to the geometry""" 

        if (name is None) and (comp.name is None): 
            name = "comp_%d"%self._n_comps
        comp.name = name
        self._i_comps[name] = self._n_comps
        self._comps.append(comp)
        self._n_comps += 1

        #rebuild the param_name_map with new comp
        self.list_parameters()
        self._invoke_callbacks()
        self._needs_linerize = True



    def deform(self,**kwargs): 
        """ deforms the geometry applying the new locations for the control points, given by body name"""
        for name,delta_C in kwargs.iteritems(): 
            i = self._i_comps[name]
            comp = self._comps[i]
            if isinstance(comp,Body):
                comp.deform(delta_C)
            else:
                comp.deform(*delta_C)

    def _build_ascii_stl(self, facets): 
        """returns a list of ascii lines for the stl file """

        lines = ['solid ffd_geom',]
        for facet in facets: 
            lines.append(ASCII_FACET.format(face=facet))
        lines.append('endsolid ffd_geom')
        return lines

    def _build_binary_stl(self, facets):
        """returns a string of binary binary data for the stl file"""

        lines = [struct.pack(BINARY_HEADER,b'Binary STL Writer',len(facets)),]
        for facet in facets: 
            facet = list(facet)
            facet.append(0) #need to pad the end with a unsigned short byte
            lines.append(struct.pack(BINARY_FACET,*facet))  
        return lines      

    def linearize(self): 
        if not self._needs_linerize: 
            return 
        self.list_parameters() #makes up to date param_loc_map

        points = []
        triangles = []
        i_offset = 0
        offsets = []
        n_controls = 0
        deriv_offsets = []
        for comp in self._comps:
            offsets.append(i_offset)
            deriv_offsets.append(n_controls)
            n_controls += sum(self.comp_param_count[comp])

            if isinstance(comp,Body): 
                points.extend(comp.stl.points)
                size = len(points)
                triangles.extend(comp.stl.triangles + i_offset) 
                i_offset = size
                #X and R for each control point, except the first X and the last R (hence the -1)
            else: 
                points.extend(comp.outer_stl.points)
                size = len(points)
                triangles.extend(comp.outer_stl.triangles + i_offset) 
                i_offset = size
                
                points.extend(comp.inner_stl.points)
                size = len(points)
                triangles.extend(comp.inner_stl.triangles + i_offset) 
                i_offset = size

        self.points = points
        self.n_controls = n_controls
        self.n_points = len(points)
        self.triangles = triangles
        self.n_triangles = len(triangles)
        self.offsets = offsets
        self.deriv_offsets = deriv_offsets

        i_comp = 0 #keep track of which comp the points came from
        comp = self._comps[0]
        i_offset=0
        i_deriv_offset = 0
        Jdata = []
        Jrow = []
        Jcolumn = []
        for i,p in enumerate(points): 
            #map point index to proper component
            if (i_comp < len(self._comps)-1) and (i == offsets[i_comp+1]): #the offset for the next comp: 
                i_offset = i
                i_comp += 1
                i_deriv_offset = deriv_offsets[i_comp]
                comp = self._comps[i_comp] 
            deriv_values = np.zeros((3*n_controls,))

            if isinstance(comp,Body): 
                size_C = self.comp_param_count[comp]
                n_C = 3*sum(size_C) 
                #x value
                start = i_deriv_offset
                end = start + 3*size_C[0] #x
                #X is only a function of the x  parameter
                X = np.array(comp.dXqdC[i-i_offset,1:])                
                deriv_values[start:end:3] = X.flatten()  

                #r value
                start = end
                end = start + 3*size_C[1] #r
                #Y,Z are only a function of the r parameter
                Y = np.array(comp.dYqdC[i-i_offset,:-1])
                Z = np.array(comp.dZqdC[i-i_offset,:-1])
                deriv_values[start+1:end:3] = Y.flatten() 
                deriv_values[start+2:end:3] = Z.flatten() 
            else: 
                size_C = self.comp_param_count[comp]
                n_C = 3*sum(size_C)
                #centerline x value
                start = i_deriv_offset
                end = start + 3*size_C[0] #x
                #determine if point is on upper or lower surface? 
                outer=True
                deriv_i = i-i_offset
                if i-i_offset >= comp.n_outer: 
                    outer=False
                    deriv_i = i-i_offset-comp.n_outer

                #X is only a function of the x  parameter
                if outer: 
                    X = np.array(comp.dXoqdCc[deriv_i,1:])
                else: 
                    X = np.array(comp.dXiqdCc[deriv_i,1:])

                deriv_values[start:end:3] = X.flatten()  

                #centerline r value
                start = end
                end = start + 3*size_C[1] #r
                #Y,Z are only a function of the r parameter
                if outer: 
                    Y = np.array(comp.dYoqdCc[deriv_i,:])
                    Z = np.array(comp.dZoqdCc[deriv_i,:])
                else: 
                    Y = np.array(comp.dYiqdCc[deriv_i,:])
                    Z = np.array(comp.dZiqdCc[deriv_i,:])
                deriv_values[start+1:end:3] = Y.flatten() 
                deriv_values[start+2:end:3] = Z.flatten() 

                #thickness parameter
                start = end
                end = start + 3*size_C[2] #t
                if outer: 
                    Y = np.array(comp.dYoqdCt[deriv_i,:-1])
                    Z = np.array(comp.dZoqdCt[deriv_i,:-1])
                else: 
                    Y = np.array(comp.dYiqdCt[deriv_i,:-1])
                    Z = np.array(comp.dZiqdCt[deriv_i,:-1])
                deriv_values[start+1:end:3] = Y.flatten() 
                deriv_values[start+2:end:3] = Z.flatten() 

            Jdata.append(deriv_values) 
        self.J = np.array(Jdata) #weird format used for tecplot fepoint, x,y,z interlaced
        self.Jx = self.J[:,0::3]
        self.Jy = self.J[:,1::3]
        self.Jz = self.J[:,2::3]

        self.JxT = self.Jx.T
        self.JyT = self.Jy.T
        self.JzT = self.Jy.T

        self._needs_linerize = False

    def writeSTL(self, file_name, ascii=False): 
        """outputs an STL file"""
        
        facets = []
        for comp in self._comps: 
            if isinstance(comp,Body): 
               facets.extend(comp.stl.get_facets())
            else: 
               facets.extend(comp.outer_stl.get_facets())
               facets.extend(comp.inner_stl.get_facets())

        f = open(file_name,'w')
        if ascii: 
            lines = self._build_ascii_stl(facets)
            f.write("\n".join(lines))
        else: 
            data = self._build_binary_stl(facets)
            f.write("".join(data))

        f.close()

    def writeFEPOINT(self, file_name): 
        """writes out a new FEPOINT file with the given name, using the supplied points.
        derivs is of size (3,len(points),len(control_points)), giving matricies of 
        X,Y,Z drivatives

        jacobian should have a shape of (len(points),len(control_points))"""
        
        self.linearize()
  
        
        lines = ['TITLE = "FFD_geom"',]
        var_line = 'VARIABLES = "X" "Y" "Z" "ID" '

        
        deriv_names = []
        deriv_tmpl = string.Template('"dx_d${name}_${type}$i" "dy_d${name}_${type}$i" "dz_d${name}_${type}$i"')
        for comp in self._comps: 
            if isinstance(comp,Body): 
                deriv_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'X_'}) for i in xrange(0,comp.n_controls-1)]) #x,y,z derivs for each control point
                deriv_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'R_'}) for i in xrange(0,comp.n_controls-1)]) #x,y,z derivs for each control point
            else: 
                deriv_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'CX_'}) for i in xrange(0,comp.n_c_controls-1)]) #x,y,z derivs for each control point
                deriv_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'CR_'}) for i in xrange(0,comp.n_c_controls)]) #x,y,z derivs for each control point
                deriv_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'T_'}) for i in xrange(0,comp.n_t_controls-1)]) #x,y,z derivs for each control point

        var_line += " ".join(deriv_names)

        lines.append(var_line)

        lines.append('ZONE T = group0, I = %d, J = %d, F=FEPOINT'%(self.n_points,self.n_triangles)) #TODO I think this J number depends on the number of variables
        
        for i,p in enumerate(self.points): 
            line = "%.8f %.8f %.8f %d "%(p[0],p[1],p[2],i+1) #x,y,z,index coordiantes of point

            deriv_values = self.J[i]
            
            line += " ".join(np.char.mod('%.8f',deriv_values))

            lines.append(line)

        for tri in self.triangles: 
            line = "%d %d %d %d"%(tri[0]+1,tri[1]+1,tri[2]+1,tri[2]+1) #tecplot wants 1 bias indecies
            lines.append(line)


        f = open(file_name,'w')
        f.write("\n".join(lines))
        f.close()

    def project_profile(self): 
        self.linearize()

        point_sets = []
        for comp in self._comps: 
            if isinstance(comp,Body):
                p = comp.stl.points
                indecies = np.logical_and(abs(p[:,2])<.0001,p[:,1]>0)
                points = p[indecies]
                points = points[points[:,0].argsort()]
                point_sets.append(points)
            else: 
                p = comp.outer_stl.points
                indecies = np.logical_and(abs(p[:,2])<.0001,p[:,1]>0)
                points = p[indecies]
                points = points[points[:,0].argsort()]
                point_ses.append(points)

                p = comp.inner_stl.points
                indecies = np.logical_and(abs(p[:,2])<.0001,p[:,1]>0)
                points = p[indecies]
                points = points[points[:,0].argsort()]
                point_sets.append(points)

        return point_sets

    #begin methods for OpenMDAO geometry derivatives
    def apply_deriv(self, arg, result): 
        self.linearize()

        for name, value in arg.iteritems(): 
            i_start = self.param_loc_map[name]
            length = value.shape[0]
            sub_Jx = self.Jx[:,i_start:i_start+length]
            result['geom_out'][:,0] += sub_Jx.dot(value)

            sub_Jy = self.Jy[:,i_start:i_start+length]
            result['geom_out'][:,1] += sub_Jy.dot(value)

            sub_Jz = self.Jz[:,i_start:i_start+length]
            result['geom_out'][:,2] += sub_Jy.dot(value)

        return result

    def apply_derivT(self, arg, result): 
        self.linearize()

        if 'geom_out' in arg: 
            for name, value in result.iteritems(): 

                i_start = self.param_loc_map[name]
                length = value.shape[0]
                sub_JxT = self.JxT[i_start:i_start+length,:]
                result[name] += sub_JxT.dot(arg['geom_out'][:,0])

                sub_JyT = self.JyT[i_start:i_start+length,:]
                result[name] += sub_JyT.dot(arg['geom_out'][:,1])

                sub_JzT = self.JzT[i_start:i_start+length,:]
                result[name] += sub_JzT.dot(arg['geom_out'][:,2])

        return result

    #end methods for OpenMDAO geometry derivatives

    #begin methods for IParametricGeometry
    def list_parameters(self): 
        """ returns a dictionary of parameters sets key'd to component names"""

        self.param_name_map = {}
        self.param_loc_map = {} #locate columns of jacobian related to a specific parameter
        self.comp_param_count = {}
        params = []
        for comp in self._comps: 
            self.i_J = 0 #jacobian column index
            name = comp.name

            if isinstance(comp, Body): 
                val = comp.delta_C[1:,0] #holds the root x constant
                meta = {'value':val, 'iotype':'in', 'shape':val.shape, 
                'desc':"axial location of control points for the ffd"}
                tup = ('%s.X'%name, meta)
                params.append(tup)
                self.param_name_map[tup[0]] = val
                self.param_loc_map[tup[0]] = self.i_J
                n_X = val.shape[0]
                self.i_J += n_X

                val = comp.delta_C[:-1,1] #holds the tip radius constant
                meta = {'value':val, 'iotype':'in', 'shape':val.shape, 
                'desc':"radial location of control points for the ffd"}
                tup = ('%s.R'%name, meta)
                params.append(tup)
                self.param_name_map[tup[0]] = val
                self.param_loc_map[tup[0]] = self.i_J
                n_R = val.shape[0]
                self.i_J += n_R
                self.comp_param_count[comp] = (n_X,n_R)


            else: 
                val = comp.delta_Cc[1:,0] #fixes the x location of the geometry root
                meta = {'value':val, 'iotype':'in', 'shape':val.shape, 
                'desc':'axial location of the control points for the centerline of the shell'}
                tup = ('%s.X'%name, meta) 
                params.append(tup)
                self.param_name_map[tup[0]] = val
                self.param_loc_map[tup[0]] = self.i_J
                n_X = val.shape[0]
                self.i_J += n_X

                val = comp.delta_Cc[:,1] #can vary all centerlines
                meta = {'value':val, 'iotype':'in', 'shape':val.shape, 
                'desc':'radial location of the control points for the centerline of the shell'}
                tup = ('%s.R'%name, meta) 
                params.append(tup)
                self.param_name_map[tup[0]] = val
                self.param_loc_map[tup[0]] = self.i_J
                n_R = val.shape[0]
                self.i_J += n_R

                val = comp.delta_Ct[:-1,1] #except last R, to keep tip size fixed
                meta = {'value':val, 'iotype':'in', 'shape':val.shape, 
                'desc':'thickness of the shell at each axial station'}
                tup = ('%s.thickness'%name, meta) 
                params.append(tup)
                self.param_name_map[tup[0]] = val
                self.param_loc_map[tup[0]] = self.i_J
                n_T = val.shape[0]
                self.comp_param_count[comp] = (n_X,n_R,n_T)
                self.i_J += n_T


        return params

    def set_parameter(self, name, val): 
        self.param_name_map[name] = val

    def get_parameters(self, names): 
        return [self.param_name_map[n] for n in names]

    def regen_model(self): 
        for comp in self._comps: 

            #print "inside STLGroup.regen_model, plug.R is ", self.meta['plug.R']['value']
            
            #del_C = np.ones((10,2)) * 123.0
            if isinstance(comp, Body): 
                delta_C_shape = comp.delta_C.shape
                del_C = np.zeros( delta_C_shape )
                del_C[1:,0] = self.param_name_map[ '%s.X' % comp.name ]
                del_C[:-1,1] = self.param_name_map[ '%s.R' % comp.name ]
                comp.deform(delta_C=del_C)
            else:
                delta_Cc_shape = comp.delta_Cc.shape
                del_Cc = np.zeros( delta_Cc_shape )
                del_Cc[1:,0] = self.param_name_map[ '%s.X' % comp.name ]
                del_Cc[:-1,1] = self.param_name_map[ '%s.R' % comp.name ]

                delta_Ct_shape = comp.delta_Ct.shape
                del_Ct = np.zeros( delta_Ct_shape )
                del_Ct[1:,0] = self.param_name_map[ '%s.X' % comp.name ]
                del_Ct[:-1,1] = self.param_name_map[ '%s.thickness' % comp.name ]
                # need both delta_Cc and delta_Ct for shells
                comp.deform(delta_Cc=del_Cc, delta_Ct=del_Ct)


    def get_static_geometry(self): 
        return self

    def register_param_list_changedCB(self, callback):
        self._callbacks.append(callback)

    def _invoke_callbacks(self): 
        for cb in self._callbacks: 
            cb()
    #end methods for IParametricGeometry

    #methods for IStaticGeometry
    def get_visualization_data(self, wv):
        self.linearize()

        xyzs = np.array(self.points).flatten().astype(np.float32)
        tris = np.array(self.triangles).flatten().astype(np.int32)
        mins = np.min(xyzs.reshape((-1,3)), axis=0)
        maxs = np.max(xyzs.reshape((-1,3)), axis=0)

        box = [mins[0], mins[1], mins[2], maxs[0], maxs[1], maxs[2]]

        #print box

        wv.set_face_data(xyzs, tris, name="surface")


    #end methods for IStaticGeometry



class STLGroupSender(STLSender):

    def initialize(self, **kwargs):
        eye    = np.array([5.0, 0.0, 70.0], dtype=np.float32)
        center = np.array([0.0, 0.0, 0.0], dtype=np.float32)
        up     = np.array([0.0, 1.0, 0.0], dtype=np.float32)
        fov   = 30.0
        zNear = 1.0
        zFar  = 100

        bias  = 0
        self.wv.createContext(bias, fov, zNear, zFar, eye, center, up)

    @staticmethod
    def supports(obj):
        return isinstance(obj, STLGroup)

    def geom_from_obj(self, obj):
        if isinstance(obj, STLGroup):
            obj.get_visualization_data(self.wv)
        else: 
            raise RuntimeError("object must be a Geometry but is a '%s' instead"%(str(type(obj))))
            

