import struct
import string

import numpy as np

from stl import ASCII_FACET, BINARY_HEADER, BINARY_FACET

from ffd_axisymetric import Body, Shell

from openmdao.main.api import Component, VariableTree
from openmdao.lib.datatypes.api import VarTree, Array
from openmdao.lib.geometry.geom_data import GeomData



def _block_diag(arrays):
    """ Create block-diagonal matrix from `arrays`. """
    result = None
    for arr in arrays:
        arr[arr == -0] = 0  # Clean -0 for compatibilty with scipy version.
        if result is None:
            result = arr
        else:
            r_rows, r_cols = result.shape
            a_rows, a_cols = arr.shape
            result = np.vstack((np.hstack((result, np.zeros((r_rows, a_cols)))),
                                np.hstack((np.zeros((a_rows, r_cols)), arr))))
    return result


class STLGroup(Component):

    def __init__(self, geom_parts):
        super(STLGroup, self).__init__()

        self._comps = []
        self._i_comps = {}
        self._n_comps = 0

        for name,comp in geom_parts: 
            comp.name = name
            self._i_comps[name] = self._n_comps
            self._comps.append(comp)
            self._n_comps += 1
            self.add(name, VarTree(VariableTree(), iotype="in", desc="inputs for %s component"%name)) #create the input VariableTree for this comp

        io = self._build_io()
        for (comp_name, var_name), meta in io: 
            comp = self.get(comp_name)
            val = meta['value']
            del meta['value']
            comp.add(var_name, Array(val, **meta))

        #add the geometry var tree
        n_points = self.points.shape[0]
        n_tria = self.triangles.shape[0]
        self.add('geom_data', VarTree(GeomData(n_points, n_tria), iotype="out", desc="geometry points and connectivity"))
        self.geom_data.points = self.points
        self.geom_data.facets = self.triangles
        
        self._needs_linerize = True


    def _build_io(self):
        """ returns a dictionary of io sets key'd to component names"""

        self.comp_param_count = {}
        params = []
        for comp in self._comps:
            name = comp.name

            if isinstance(comp, Body):
                val = comp.delta_C[:,0] #holds the root x constant
                meta = {'value':val, 'iotype':'in', 'shape':val.shape,
                'desc':"axial location of control points for the ffd"}
                tup = ((name,'X'), meta)

                params.append(tup)
                n_X = val.shape[0]

                val = comp.delta_C[:,1] #holds the tip radius constant
                meta = {'value':val, 'iotype':'in', 'shape':val.shape,
                'desc':"radial location of control points for the ffd"}
                tup = ((name,'R'), meta)
                params.append(tup)
                n_R = val.shape[0]
                self.comp_param_count[comp] = (n_X,n_R)


            else:
                val = comp.delta_Cc[:,0] #fixes the x location of the geometry root
                meta = {'value':val, 'iotype':'in', 'shape':val.shape,
                'desc':'axial location of the control points for the centerline of the shell'}
                tup = ((name,'X'), meta)
                params.append(tup)
                n_X = val.shape[0]

                val = comp.delta_Cc[:,1] #can vary all centerlines
                meta = {'value':val, 'iotype':'in', 'shape':val.shape,
                'desc':'radial location of the control points for the centerline of the shell'}
                tup = ((name,'R'), meta)
                params.append(tup)
                n_R = val.shape[0]

                val = comp.delta_Ct[:,1] #except last R, to keep tip size fixed
                meta = {'value':val, 'iotype':'in', 'shape':val.shape,
                'desc':'thickness of the shell at each axial station'}
                tup = ((name,'thickness'), meta)
                params.append(tup)
                n_T = val.shape[0]
                self.comp_param_count[comp] = (n_X,n_R,n_T)

        #do some point book keeping here
        points = []
        triangles = []
        i_offset = 0
        n_controls = 0
        for comp in self._comps:
            n_controls += sum(self.comp_param_count[comp])

            if isinstance(comp,Body):
                points.extend(comp.stl.points)
                size = len(points)
                triangles.extend(comp.stl.triangles + i_offset)
                i_offset = size
            else:
                points.extend(comp.outer_stl.points)
                size = len(points)
                triangles.extend(comp.outer_stl.triangles + i_offset)
                i_offset = size

                points.extend(comp.inner_stl.points)
                size = len(points)
                triangles.extend(comp.inner_stl.triangles + i_offset)
                i_offset = size

        self.points = np.array(points)
        self.n_controls = n_controls
        self.n_points = len(points)
        self.triangles = np.array(triangles)
        self.n_triangles = len(triangles)

        return params

    def deform(self,**kwargs):
        """ deforms the geometry applying the new locations for the control points, given by body name"""
        for name,delta_C in kwargs.iteritems():
            i = self._i_comps[name]
            comp = self._comps[i]
            if isinstance(comp,Body):
                comp.deform(delta_C)
            else:
                comp.deform(*delta_C)
            self.list_parameters()

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

    def writeFEPOINT(self, stream):
        """writes out a new FEPOINT file with the given name, using the supplied points.
        derivs is of size (3,len(points),len(control_points)), giving matricies of
        X,Y,Z drivatives

        jacobian should have a shape of (len(points),len(control_points))"""

        self.provideJ()

        lines = ['TITLE = "FFD_geom"',]
        var_line = 'VARIABLES = "X" "Y" "Z" "ID" '


        deriv_X_names = []
        deriv_R_names = []
        deriv_T_names = []

        deriv_tmpl = string.Template('"dx_d${name}_${type}$i" "dy_d${name}_${type}$i" "dz_d${name}_${type}$i"')

        for comp in self._comps:
            if isinstance(comp,Body):
                deriv_X_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'X'}) for i in xrange(0,comp.n_controls)]) #x,y,z derivs for each control point
                deriv_R_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'R'}) for i in xrange(0,comp.n_controls)]) #x,y,z derivs for each control point

            else:
                deriv_X_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'X'}) for i in xrange(0,comp.n_c_controls)]) #x,y,z derivs for each control point
                deriv_R_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'R'}) for i in xrange(0,comp.n_c_controls)]) #x,y,z derivs for each control point
                deriv_T_names.extend([deriv_tmpl.substitute({'name':comp.name,'i':str(i),'type':'T'}) for i in xrange(0,comp.n_t_controls)]) #x,y,z derivs for each control point

        var_line += " ".join(deriv_X_names)
        var_line += " ".join(deriv_R_names)
        var_line += " ".join(deriv_T_names)

        lines.append(var_line)

        lines.append('ZONE T = group0, I = %d, J = %d, F=FEPOINT'%(self.n_points,self.n_triangles)) #TODO I think this J number depends on the number of variables

        #xyz for each parameter
        nx = 3*self.dXqdC.shape[1]
        nr = 3*self.dYqdCr.shape[1]
        nt = 3*self.dYqdCt.shape[1]
        j_cols =  (nx+nr+nt)


        for i,p in enumerate(self.points):
            line = "%.8f %.8f %.8f %d "%(p[0],p[1],p[2],i+1) #x,y,z,index coordiantes of point

            #deriv_values = self.J[i]
            deriv_values = np.zeros((j_cols,))
            deriv_values[:nx:3] = self.dXqdC[i]

            #leave x as zero
            deriv_values[nx+1:nx+nr:3] = self.dYqdCr[i]
            deriv_values[nx+2:nx+nr:3] = self.dZqdCr[i]

            #leave x as zero
            deriv_values[nx+nr+1::3] = self.dYqdCt[i]
            deriv_values[nx+nr+2::3] = self.dZqdCt[i]

            line += " ".join(np.char.mod('%.8f',deriv_values))
            lines.append(line)

        for tri in self.triangles:
            line = "%d %d %d %d"%(tri[0]+1,tri[1]+1,tri[2]+1,tri[2]+1) #tecplot wants 1 bias indecies
            lines.append(line)


        needs_close = False
        if isinstance(stream, basestring):
            stream = open(stream,'w')
            needs_close = True

        #f.write("\n".join(lines))
        #f.close()

        print >> stream, "\n".join(lines)
        if(needs_close):
            stream.close()

    def project_profile(self):
        self.provideJ()

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
                point_sets.append(points)

                p = comp.inner_stl.points
                indecies = np.logical_and(abs(p[:,2])<.0001,p[:,1]>0)
                points = p[indecies]
                points = points[points[:,0].argsort()]
                point_sets.append(points)

        return point_sets

    #begin methods for OpenMDAO geometry derivatives
    def list_deriv_vars(self):
        outs = []
        ins = []
        for comp in self._comps:
            ins.append('%s.X'%comp.name)
            ins.append('%s.R'%comp.name)
            if isinstance(comp, Shell):
                ins.append('%s.thickness'%comp.name)

        return ins, outs

    def provideJ(self):
        if not self._needs_linerize:
            return
        self._build_io()

        param_J_offset_map = {}

        #get proper sub_jacobians:
        jx = []
        jyr = []
        jzr = []
        jyt = [] #thickness jacobian for shells
        jzt = [] #thickness jacobian for shells

        x_offset = 0
        yz_offset = 0
        t_offset = 0
        for comp in self._comps:
            if isinstance(comp, Body):
                jx.append(comp.dXqdC)
                param_name = "%s.X"%comp.name
                param_J_offset_map[param_name] = x_offset
                nCx = self.comp_param_count[comp][0]
                x_offset += nCx

                jyr.append(comp.dYqdC) 
                jzr.append(comp.dZqdC)
                param_name = "%s.R"%comp.name
                param_J_offset_map[param_name] = yz_offset
                nCr = self.comp_param_count[comp][1]
                yz_offset += nCr

                #zeros for thickness n_pointsx1
                shape = comp.dXqdC.shape
                param_name = "%s.thickness"%comp.name #note: this parameter does not exists, so I'll remove the columns from the jacobian
                jyt.append(np.zeros((shape[0],1)))
                jzt.append(np.zeros((shape[0],1)))
                param_J_offset_map[param_name] = t_offset
                t_offset += 1

            else:
                #inner and outer jacobians
                #have to stack the outer and inner jacobians
                stackX = np.vstack((comp.dXoqdCc, comp.dXiqdCc))
                jx.append(stackX)
                param_name = "%s.X"%comp.name
                param_J_offset_map[param_name] = x_offset
                nCx = self.comp_param_count[comp][0]
                x_offset += nCx

                #centerline
                stackY = np.vstack((comp.dYoqdCc, comp.dYiqdCc))
                stackZ = np.vstack((comp.dZoqdCc, comp.dZiqdCc))
                jyr.append(stackY) #constant tip radius
                jzr.append(stackZ)
                param_name = "%s.R"%comp.name
                param_J_offset_map[param_name] = yz_offset
                nCr = self.comp_param_count[comp][1]
                yz_offset += nCr

                #thickness
                stackY = np.vstack((comp.dYoqdCt, comp.dYiqdCt))
                stackZ = np.vstack((comp.dZoqdCt, comp.dZiqdCt))
                jyt.append(stackY) #constant tip radius
                jzt.append(stackZ)
                param_name = "%s.thickness"%comp.name
                param_J_offset_map[param_name] = t_offset
                nCt = self.comp_param_count[comp][2]
                t_offset += nCt

        self.dXqdC = _block_diag(jx)
        self.dYqdCr = _block_diag(jyr)
        self.dZqdCr = _block_diag(jzr)
        self.dYqdCt = _block_diag(jyt)
        self.dZqdCt = _block_diag(jzt)


        self.param_J_map = {}
        #map param names to jacobians:
        for comp in self._comps:
            param_name = "%s.X"%comp.name
            offset = param_J_offset_map[param_name]
            nCx = self.comp_param_count[comp][0]
            self.param_J_map[param_name] = (self.dXqdC[:,offset:offset+nCx], False, False)

            param_name = "%s.R"%comp.name
            offset = param_J_offset_map[param_name]
            nCr = self.comp_param_count[comp][1]
            self.param_J_map[param_name] = (False, self.dYqdCr[:,offset:offset+nCr], self.dZqdCr[:,offset:offset+nCr])

            if isinstance(comp, Shell):
                param_name = "%s.thickness"%comp.name
                offset = param_J_offset_map[param_name]
                nCt = self.comp_param_count[comp][2]
                self.param_J_map[param_name] = (False, self.dYqdCt[:,offset:offset+nCt], self.dZqdCt[:,offset:offset+nCt])

        #go through and remove the extra columns from fake body thicknesses
        for comp in self._comps:
            if isinstance(comp, Body):
                param_name = "%s.thickness"%comp.name
                offset = param_J_offset_map[param_name]

                self.dYqdCt = np.delete(self.dYqdCt,offset,1)
                self.dZqdCt = np.delete(self.dZqdCt,offset,1)

        self._needs_linerize = False

    def apply_deriv(self, arg, result):
        for name, value in arg.iteritems():
            Jx, Jy, Jz = self.param_J_map[name]
            if Jx is not False:
                result['geom_data.points'][:,0] += Jx.dot(value)
            if Jy is not False:
                result['geom_data.points'][:,1] += Jy.dot(value)
                result['geom_data.points'][:,2] += Jz.dot(value)

        return result

    def apply_derivT(self, arg, result):

        for name, value in result.iteritems():
            Jx, Jy, Jz = self.param_J_map[name]
            if Jx is not False:
                result[name] += Jx.T.dot(arg['geom_data.points'][:,0])
            if Jy is not False:
                result[name] += Jy.T.dot(arg['geom_data.points'][:,1])
                result[name] += Jz.T.dot(arg['geom_data.points'][:,2])

        return result




    def execute(self):
        for comp in self._comps:

            #print "inside STLGroup.regen_model, plug.R is ", self.meta['plug.R']['value']

            #del_C = np.ones((10,2)) * 123.0

            if isinstance(comp, Body):
                delta_C_shape = comp.delta_C.shape
                del_C = np.zeros( delta_C_shape )
                del_C[:,0] = self.get('%s.X' % comp.name)
                del_C[:,1] = self.get('%s.R' % comp.name)
                comp.deform(delta_C=del_C)
            else:
                delta_Cc_shape = comp.delta_Cc.shape
                del_Cc = np.zeros( delta_Cc_shape )
                del_Cc[:,0] = self.get('%s.X' % comp.name)
                del_Cc[:,1] = self.get('%s.R' % comp.name)

                delta_Ct_shape = comp.delta_Ct.shape
                del_Ct = np.zeros( delta_Ct_shape )
                del_Ct[:,0] = self.get('%s.X' % comp.name)
                del_Ct[:,1] = self.get('%s.thickness' % comp.name)
                # need both delta_Cc and delta_Ct for shells
                comp.deform(delta_Cc=del_Cc, delta_Ct=del_Ct)


        self._build_io() #needed for book-keeping
        self.geom_data.points = self.points
        self.geom_data.facets = self.triangles



