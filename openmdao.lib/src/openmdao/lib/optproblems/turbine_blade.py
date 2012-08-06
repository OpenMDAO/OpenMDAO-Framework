"""
Two discipline Turbine Blade Design Problem. Considers thermal and structural disciplines

J. Allison, "Optimal partitioning and coordination decisions in decomposition-based design optimization",
Univeristy of Michigan, 2008.

"""

from math import cosh, tanh, exp

from openmdao.main.api import Component, VariableTree, Slot, SequentialWorkflow
from openmdao.main.problem_formulation import OptProblem
from openmdao.lib.datatypes.api import Float, Array

class Constants(VariableTree): 
    #From Table 3.1 in reference
    rho = Float(8510.0, units="kg/m**3")
    L0 = Float(0.05, units="m") 
    alpha = Float(12.6e-6, units="m/degK")
    r = Float(0.5, units="m")
    omega = Float(2100,  units="rad/s")
    delta_max = Float(0.05,  units="m")
    rho_g = Float(3.522,  units="kg/m**3")
    C_d = Float(2.0, iotype="in") 
    v = Float(100 ,  units="m/s")
    T_b = Float(300 ,  units="degC")
    T_inf = Float(900 ,  units = "degC")
    epsilon = Float(1.0e-8 ) 
    

class Thermal(Component): 
    
    constants = Slot(Constants,iotype="in")
    
    w = Float(0.08,iotype="in",desc="blade width", low=0)
    t = Float(0.005,iotype="in",desc="blade thickness", low=0)
    L = Float(0.05,iotype="in",desc="blade length", low=0)
    
    q = Float(0.0, iotype="out", desc="heat transfer through the blade into the hub")
    
    T_x = Array(default_value=[400,400,400,400,400,400,400,400],dtype=float,iotype="out"
                ,desc="Temperature distribution along x direction")
    X =   Array(default_value=[0,0,0,0,0,0,0,0],dtype=float,iotype="out"
                ,desc="discritization in the x direction")
    
    def execute(self): 
        h_bar = 9.196*(self.constants.v**0.731)*(self.w**-0.269) #eqn 3.14
        
        k = 6.8024 + 0.0172*(self.constants.T_b+self.constants.T_inf)/2 #eqn 3.16
        s = (2*h_bar*(self.t+self.w)/(k*self.t*self.w))**0.5 #eqn 3.14))
        self.q = self.w*self.t*(self.constants.T_b-self.constants.T_inf)* \
                 tanh(s*self.L)*(2*h_bar*(self.t+self.w)*k*self.t*self.w)**0.5
        
        #print "T_x: ",
        for i in range(0,len(self.T_x)): #discritized Temperature distribution   
            self.X[i] = (1.0/(len(self.T_x)-1)*i)*self.L
            self.T_x[i] = cosh(s*(self.L-self.X[i]))/cosh(s*self.L)*\
                          (self.constants.T_b-self.constants.T_inf) + self.constants.T_inf
            #print self.T_x[i],
            

class Structures(Component): 
    constants = Slot(Constants,iotype="in")
    
    T_x = Array(default_value=[400,400,400,400,400,400,400,400],dtype=float,iotype="in"
                ,desc="Temperature distribution along x direction")
    X =   Array(default_value=[0,0,0,0,0,0,0,0],dtype=float,iotype="in"
                ,desc="discritization in the x direction")
    
    t = Float(0.005,iotype="in",desc="blade thickness", low=0)
    w = Float(0.08,iotype="in",desc="blade width", low=0)    
    
    delta_total = Float(0.001,iotype="in",desc="state variable for blade elongation",low=-10,high=1000)
    delta_total_residual = Float(0.0,iotype="out",desc="residual state for blade elongation")
    
    sigma_a = Array(default_value=[0,0,0,0,0,0,0,0],dtype=float,iotype="out",
                    desc="discritized axial stress")
    sigma_b = Array(default_value=[0,0,0,0,0,0,0,0],dtype=float,iotype="out",
                        desc="discritized bending stress") 
    sigma_r = Array(default_value=[0,0,0,0,0,0,0,0],dtype=float,iotype="out",
                            desc="discritized rupture stress constraint")     
    L = Float(0.0,iotype="out",desc="blade length")
    m = Float(0.0,iotype="out",desc="blade mass")
    
    #eqn 3.17
    def _E(self,T): 
        return 209.8 - 0.0487*T - 0.0002*T**2 + 6e-7*T**3 - 6e-10*T**4
    
    def _ax(self,x,t): 
        return (self.constants.L0+self.delta_total-x)/(self._E(t))
    
    def execute(self):         
        self.m = self.constants.rho*self.constants.L0*self.w*self.t
        self.L = self.constants.L0 + self.delta_total    
        K = .5*self.constants.rho*self.constants.C_d*self.constants.v**2        
        
        delta_th = -1*self.constants.alpha*self.constants.L0*self.constants.T_b #discritized version of eqn. 3.7
        delta_ax = 0
        
        s = 0
        for i,(t1,x1,t2,x2) in enumerate(zip(self.T_x[:-1],self.X[:-1],self.T_x[1:],self.X[1:])):   
            dx = (x2-x1)
            x_avg = (x1+x2)/2.0
            t_avg = (t1+t2)/2.0
            s+= self._ax(x_avg,t_avg)*dx*(self.constants.omega**2)*self.constants.r*self.constants.rho
            print s
            delta_th += t_avg*dx #discritized version of eqn. 3.7
            delta_ax += (self.constants.omega**2)*self.constants.r*self.constants.rho*\
                        (self.constants.L0+self.delta_total-x_avg)/self._E(t_avg)*dx #discritized version of eqn 3.9
            
            self.sigma_r[i] = 1300/(1+exp(0.011*(t_avg-675))) #eqn 3.15
            self.sigma_a[i] = self.constants.omega**2*self.constants.r*self.constants.rho*(self.L-x_avg) #eqn 3.11
            self.sigma_b[i] = (3*K*(self.L-x_avg)**2)/(4*self.t**2) #eqn 3.12
            
        #eqn 3.10
        print "Test: ", delta_ax, delta_th
        delta_total_calc = delta_th+delta_ax
        self.delta_total_residual = delta_total_calc - self.delta_total
              
            
    
    
from openmdao.lib.drivers.api import BroydenSolver

class TurbineBlade(OptProblem): 
    constants = Slot(Constants,iotype="in")
    
    
    def configure(self): 
        self.constants = Constants() #initialization
        
        self.add('therm',Thermal())
        self.add('struct',Structures())
        self.connect('constants',['therm.constants','struct.constants'])
        
        #self.add_parameter(['therm.t','struct.t'],name="t",start=0.002)
        #self.add_parameter(['therm.w','struct.w'],name="w",start=0.1)
        
        #self.add_coupling_var(('struct.T_x[0]','therm.T_x[0]'),name='T_x0',start=400)
        #self.add_coupling_var(('struct.T_x[1]','therm.T_x[1]'),name='T_x1',start=400)
        #self.add_coupling_var(('struct.T_x[2]','therm.T_x[2]'),name='T_x2',start=400)
        #self.add_coupling_var(('struct.T_x[3]','therm.T_x[3]'),name='T_x3',start=400)
        #self.add_coupling_var(('struct.T_x[4]','therm.T_x[4]'),name='T_x4',start=400)
        #self.add_coupling_var(('struct.T_x[5]','therm.T_x[5]'),name='T_x5',start=400)
        
        #self.add_coupling_var(('therm.L','struct.L'),name='L',start=0.05)
        
        #self.add_objective('therm.q')
        
        
        self.add('driver',BroydenSolver())
        self.driver.workflow=SequentialWorkflow()
        self.driver.workflow.add(['therm','struct'])
        self.driver.add_parameter('struct.delta_total')
        self.driver.add_parameter('therm.L')
        self.driver.add_constraint('0 = struct.delta_total_residual')
        self.driver.add_constraint('struct.L=therm.L')
        self.connect('therm.T_x[0]','struct.T_x[0]')
        self.connect('therm.T_x[1]','struct.T_x[1]')
        self.connect('therm.T_x[2]','struct.T_x[2]')
        self.connect('therm.T_x[3]','struct.T_x[3]')
        self.connect('therm.T_x[4]','struct.T_x[4]')
        self.connect('therm.T_x[5]','struct.T_x[5]')
        
        self.connect('therm.X','struct.X')
     
        
if __name__ == "__main__": 
    
    s = Structures()
    t = Thermal()
    p = TurbineBlade()
    
    p.therm.w = p.struct.w = 0.08
    p.therm.t = p.struct.t = 0.005
    
    p.run()
    
    print p.therm.T_x
    print p.therm.X
    print 
    print p.therm.q
    print p.struct.m
    print p.struct.L
        
        
        
        