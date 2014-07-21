import copy

import numpy as np

from bspline import Bspline


class Coordinates(object): 
    """transforms points from Cartesian space to cylindrical space and vice versa"""

    def __init__(self,points,cartesian=True): 
        """cartisian flag indicates which type of coordinates are begin given""" 

        #cartesian: x along the length; y,z along the thickness
        #cylindrical: x along the axis; r,theta along the thickness
        points = np.array(points,dtype=np.float64)

        if cartesian: 
            self.cartesian = points

            X,Y,Z = points[:,0],points[:,1],points[:,2]
            R = np.sqrt(Y**2+Z**2)
            Theta = np.arctan2(Y,Z)
            self.cylindrical = np.nan_to_num(np.vstack((X,R,Theta)).T)
        else: 
            self.cylindrical = points
            X,R,Theta = points[:,0],points[:,1],points[:,2]
            Z = R*np.cos(Theta)
            Y = R*np.sin(Theta)
            self.cartesian = np.nan_to_num(np.vstack((X,Y,Z)).T)


class Body(object): 
    """FFD class for solid bodies which only have one surface""" 
    
    def __init__(self,stl,controls,name="body", r_ref=None, x_ref=None): 
        """stl must be an STL object"""

        self.stl = stl
        geom_points = stl.points

        self.coords = Coordinates(geom_points,cartesian=True)

        self.P = self.coords.cylindrical
        self.P_cart = self.coords.cartesian
        self.P_bar = geom_points.copy() #just initialization
        if isinstance(controls,int): 
            X = geom_points[:,0]
            x_max = np.max(X)
            x_min = np.min(X)
            C_x = np.linspace(x_min,x_max,controls) 
            C_r = np.zeros((controls,))
            control_points = np.array(zip(C_x,C_r))

            self.C = control_points 
            self.n_controls = controls
        else: 
            self.C = controls
            self.n_controls = len(control_points)
        self.C_bar = self.C.copy()
        self.delta_C = np.zeros(self.C.shape)
        self.bs = Bspline(self.C,geom_points)

        self.name = name

        if x_ref is not None: 
            self.x_mag = float(x_ref)
        else: 
            self.x_mag = 10**np.floor(np.log10(np.average(geom_points[:,0])))

        if r_ref is not None: 
            self.r_mag = float(r_ref)
        else: 
            indecies = np.logical_and(abs(geom_points[:,2])<.0001, geom_points[:,1]>0)
            points = geom_points[indecies]
            self.r_mag = 10**np.floor(np.log10(np.average(points[:,1]))) #grab the order of magnitude of the average
        

        #for revolution of 2-d profile
        #self.n_theta = 20

        #sgrab the theta values from the points 
        self.Theta = self.P[:,2]
        #this is too complex. shouldn't need to tile, then flatten later.
        self.sin_theta = np.tile(np.sin(self.Theta),(self.n_controls,1)).T.flatten()
        self.cos_theta = np.tile(np.cos(self.Theta),(self.n_controls,1)).T.flatten()
        # self.sin_theta = np.tile(np.sin(self.Theta),self.n_controls)
        # self.cos_theta = np.tile(np.cos(self.Theta),self.n_controls)

        #calculate derivatives
        #in polar coordinates
        self.dP_bar_xqdC = np.array(self.x_mag*self.bs.B.flatten())
        self.dP_bar_rqdC = np.array(self.r_mag*self.bs.B.flatten())

        #Project Polar derivatives into revolved cartisian coordinates
        self.dXqdC = self.dP_bar_xqdC.reshape(-1,self.n_controls)
        self.dYqdC = (self.dP_bar_rqdC*self.sin_theta).reshape(-1,self.n_controls)
        self.dZqdC = (self.dP_bar_rqdC*self.cos_theta).reshape(-1,self.n_controls)

    def copy(self): 
        return copy.deepcopy(self)

    def deform(self,delta_C): 
        """returns new point locations for the given motion of the control 
        points""" 
        self.delta_C = delta_C  
        self.delta_C[:,0] = self.delta_C[:,0]*self.x_mag
        self.C_bar = self.C+self.delta_C
        delta_P = self.bs.calc(self.C_bar)

        self.P_bar = self.P.copy()
        self.P_bar[:,0] = delta_P[:,0]
        self.P_bar[:,1] = self.P[:,1]+self.r_mag*delta_P[:,1]

        #transform to cartesian coordinates
        self.coords = Coordinates(self.P_bar,cartesian=False)

        self.P_bar_cart = self.coords.cartesian
        self.Xo = self.P_bar_cart[:,0]
        self.Yo = self.P_bar_cart[:,1]
        self.Zo = self.P_bar_cart[:,2]

        self.stl.update_points(self.P_bar_cart)

        return self.P_bar


                    
        
class Shell(object): 
    """FFD class for shell bodies which have two connected surfaces"""
    
    def __init__(self, outer_stl, inner_stl, center_line_controls,
        thickness_controls, name='shell', r_ref=None, x_ref=None): 

        self.outer_stl = outer_stl
        self.inner_stl = inner_stl

        outer_points = outer_stl.points
        inner_points = inner_stl.points

        self.n_outer = len(outer_points)
        self.n_inner = len(inner_points)

        self.outer_coords = Coordinates(outer_points, cartesian=True)
        self.inner_coords = Coordinates(inner_points, cartesian=True)
    
        self.Po = self.outer_coords.cylindrical
        self.Pi = self.inner_coords.cylindrical
        self.Po_cart = self.outer_coords.cartesian
        self.Pi_cart = self.inner_coords.cartesian
        #just initialization for array size
        self.Po_bar = outer_points.copy()
        self.Pi_bar = inner_points.copy()
        self.name = name
        
        if isinstance(center_line_controls,int): 
            X = outer_points[:,0]
            x_max = np.max(X)
            x_min = np.min(X)
            C_x = np.linspace(x_min,x_max,center_line_controls) 
            C_r = np.zeros((center_line_controls,))
            control_points = np.array(zip(C_x,C_r))

            self.Cc = control_points
            self.n_c_controls = center_line_controls
        else: 
            self.Cc = center_line_controls
            self.n_c_controls = len(center_line_controls)
        self.Cc_bar = self.Cc.copy()
        self.delta_Cc = np.zeros(self.Cc.shape)


        if isinstance(thickness_controls,int): 
            X = inner_points[:,0]
            x_max = np.max(X)
            x_min = np.min(X)
            C_x = np.linspace(x_min,x_max,thickness_controls) 
            C_r = np.zeros((thickness_controls,))
            control_points = np.array(zip(C_x,C_r))
            self.Ct = control_points
            self.n_t_controls = thickness_controls
        else: 
            self.Ct = thickness_controls 
            self.n_t_controls = len(thickness_controls)
        self.Ct_bar = self.Ct.copy()
        self.delta_Ct = np.zeros(self.Ct.shape)
         
        self.bsc_o = Bspline(self.Cc,outer_points)
        self.bsc_i = Bspline(self.Cc,inner_points)
        
        self.bst_o = Bspline(self.Ct,outer_points)
        self.bst_i = Bspline(self.Ct,inner_points)
        
        self.name = name

        if x_ref is not None: 
            self.x_mag = float(x_ref)
        else: 
            self.x_mag = 10**np.floor(np.log10(np.average(outer_points[:,0])))

        if r_ref is not None: 
            self.r_mag = float(r_ref)
        else: 
            indecies = np.logical_and(abs(outer_points[:,2])<.0001, outer_points[:,1]>0)
            points = outer_points[indecies]
            self.r_mag = 10**np.floor(np.log10(np.average(points[:,1]))) #grab the order of magnitude of the average


        self.outer_theta = self.Po[:,2]
        self.sin_outer_c_theta = np.tile(np.sin(self.outer_theta),(self.n_c_controls,1)).T.flatten()
        self.cos_outer_c_theta = np.tile(np.cos(self.outer_theta),(self.n_c_controls,1)).T.flatten()
        self.sin_outer_t_theta = np.tile(np.sin(self.outer_theta),(self.n_t_controls,1)).T.flatten()
        self.cos_outer_t_theta = np.tile(np.cos(self.outer_theta),(self.n_t_controls,1)).T.flatten()

        self.inner_theta = self.Pi[:,2]
        self.sin_inner_c_theta = np.tile(np.sin(self.inner_theta),(self.n_c_controls,1)).T.flatten()
        self.cos_inner_c_theta = np.tile(np.cos(self.inner_theta),(self.n_c_controls,1)).T.flatten()
        self.sin_inner_t_theta = np.tile(np.sin(self.inner_theta),(self.n_t_controls,1)).T.flatten()
        self.cos_inner_t_theta = np.tile(np.cos(self.inner_theta),(self.n_t_controls,1)).T.flatten()

        #calculate derivatives
        #in polar coordinates
        self.dPo_bar_xqdCc = np.array(self.x_mag*self.bsc_o.B.flatten())
        self.dPo_bar_rqdCc = np.array(self.r_mag*self.bsc_o.B.flatten())

        self.dPi_bar_xqdCc = np.array(self.x_mag*self.bsc_i.B.flatten())
        self.dPi_bar_rqdCc = np.array(self.r_mag*self.bsc_i.B.flatten())

        self.dPo_bar_rqdCt = np.array(self.r_mag*self.bst_o.B.flatten())
        self.dPi_bar_rqdCt = -1*np.array(self.r_mag*self.bst_i.B.flatten())

        #Project Polar derivatives into revolved cartisian coordinates
        self.dXoqdCc = self.dPo_bar_xqdCc.reshape(-1,self.n_c_controls)
        self.dYoqdCc = (self.dPo_bar_rqdCc*self.sin_outer_c_theta).reshape(-1,self.n_c_controls)
        self.dZoqdCc = (self.dPo_bar_rqdCc*self.cos_outer_c_theta).reshape(-1,self.n_c_controls)

        self.dXiqdCc = self.dPi_bar_xqdCc.reshape(-1,self.n_c_controls)
        self.dYiqdCc = (self.dPi_bar_rqdCc*self.sin_inner_c_theta).reshape(-1,self.n_c_controls)
        self.dZiqdCc = (self.dPi_bar_rqdCc*self.cos_inner_c_theta).reshape(-1,self.n_c_controls)

        self.dYoqdCt = (self.dPo_bar_rqdCt*self.sin_outer_t_theta).reshape(-1,self.n_t_controls)
        self.dZoqdCt = (self.dPo_bar_rqdCt*self.cos_outer_t_theta).reshape(-1,self.n_t_controls)
        self.dYiqdCt = (self.dPi_bar_rqdCt*self.sin_inner_t_theta).reshape(-1,self.n_t_controls)
        self.dZiqdCt = (self.dPi_bar_rqdCt*self.cos_inner_t_theta).reshape(-1,self.n_t_controls)

    def copy(self): 
        return copy.deepcopy(self)

    def plot_geom(self,ax,initial_color='g',ffd_color='k'):
        if initial_color: 
            ax.scatter(self.Po[:,0],self.Po[:,1],c=initial_color,s=50,label="%s initial geom"%self.name)
            ax.scatter(self.Pi[:,0],self.Pi[:,1],c=initial_color,s=50)
            ax.plot(self.Po[:,0],self.Po[:,1],c=initial_color) 
            ax.plot(self.Pi[:,0],self.Pi[:,1],c=initial_color) 
        if ffd_color: 
            ax.scatter(self.Po_bar[:,0],self.Po_bar[:,1],c=ffd_color,s=50,label="%s ffd geom"%self.name) 
            ax.scatter(self.Pi_bar[:,0],self.Pi_bar[:,1],c=ffd_color,s=50) 
            ax.plot(self.Po_bar[:,0],self.Po_bar[:,1],c=ffd_color) 
            ax.plot(self.Pi_bar[:,0],self.Pi_bar[:,1],c=ffd_color) 

    def plot_centerline_spline(self,ax,point_color='r',line_color='b'):
        ax.scatter(self.Cc_bar[:,0],self.Cc_bar[:,1],c=point_color,s=50,label="%s Centerline Control Points"%self.name)
        map_points = self.bsc_o(np.linspace(0,1,100))
        ax.plot(map_points[:,0],map_points[:,1],label="Centerline b-spline Curve",c=line_color)

    def plot_thickness_spline(self,ax,point_color='r',line_color='b'):
        ax.scatter(self.Ct_bar[:,0],self.Ct_bar[:,1],c=point_color,s=50,label="%s Thickness Control Points"%self.name)
        map_points = self.bst_o(np.linspace(0,1,100))
        ax.plot(map_points[:,0],map_points[:,1],label="Thickness b-spline Curve",c=line_color)


    def deform(self,delta_Cc,delta_Ct): 
        """returns new point locations for the given motion of the control 
        points for center-line and thickness"""      
        
        self.delta_Cc = delta_Cc
        self.delta_Cc[:,0]*=self.x_mag

        self.Cc_bar = self.Cc+self.delta_Cc
        delta_Pc_o = self.bsc_o.calc(self.Cc_bar)
        delta_Pc_i = self.bsc_i.calc(self.Cc_bar)
        
        self.delta_Ct = delta_Ct
        self.Ct_bar = self.Ct+self.delta_Ct
        delta_Pt_o = self.bst_o.calc(self.Ct_bar)
        delta_Pt_i = self.bst_i.calc(self.Ct_bar)

        self.Po_bar = self.Po.copy()
        self.Pi_bar = self.Pi.copy()
        
        self.Po_bar[:,0] = delta_Pc_o[:,0]
        self.Po_bar[:,1] = self.Po[:,1]+self.r_mag*(delta_Pc_o[:,1]+delta_Pt_o[:,1])
        
        self.Pi_bar[:,0] = delta_Pc_i[:,0]
        self.Pi_bar[:,1] = self.Pi[:,1]+self.r_mag*(delta_Pc_i[:,1]-delta_Pt_i[:,1])

        #transform to cartesian coordinates
        self.outer_coords = Coordinates(self.Po_bar,cartesian=False)
        self.inner_coords = Coordinates(self.Pi_bar,cartesian=False)
        
        #Perform axial roation of 2-d polar coordiantes
        #outer surface
        self.Po_bar_cart = self.outer_coords.cartesian
        self.Xo = self.Po_bar_cart[:,0]
        self.Yo = self.Po_bar_cart[:,1]
        self.Zo = self.Po_bar_cart[:,2]

        self.outer_stl.update_points(self.Po_bar_cart)

        #inner surface
        self.Pi_bar_cart = self.inner_coords.cartesian
        self.Xi = self.Po_bar_cart[:,0]
        self.Yi = self.Po_bar_cart[:,1]
        self.Zi = self.Po_bar_cart[:,2]

        self.inner_stl.update_points(self.Pi_bar_cart)

        return self.Po_bar,self.Pi_bar

if __name__ == "__main__":
    p = [[0,0,0],[0,0,1],[0,1,0]]     
    p_prime = Coordinates(p,cartesian=True)
    print p_prime.cartesian
    print p_prime.cylindrical   
    
    p = [[0,0,0],[0,1,0],[0,1,np.pi/2]]
    p_prime = Coordinates(p,cartesian=False)
    print p_prime.cartesian
    print p_prime.cylindrical      



        