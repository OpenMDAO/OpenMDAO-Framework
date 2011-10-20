from openmdao.main.api import Component
from openmdao.main.problem_formulation import OptProblem

from openmdao.lib.datatypes.api import Float

class W(Component): 
    
    x_w = Float(0.0,iotype="in")
    y2 = Float(0.0,iotype="in")
    z1 = Float(0.0,iotype="in")
    
    w1 = Float(0.0,iotype="out")
    w2 = Float(0.0,iotype="out")
    w3 = Float(0.0,iotype="out")
    g_w = Float(0.0,iotype="out")

    
    
    def execute(self): 
        x = self.x_w
        y = self.y2 
        z = self.z1 
        
        self.w1 = 0.22*x+0.05*y**3-0.46*z**2+0.73*(x*y)
        self.w2 = -0.96*y**2+0.56*z**3-0.03*(x*y)**2
        self.w3 = 0.36*x**2 + 0.93*(z*y)**2
        self.g_w = -578.9+0.36*y**3+0.55*x+0.09*(x*z)**3

class Y(Component): 
    
    x_y = Float(0.0,iotype="in")
    w1 = Float(0.0,iotype="in")
    w3 = Float(0.0,iotype="in")
    
    y1 = Float(0.0,iotype="out")
    y2 = Float(0.0,iotype="out")
    g_y = Float(0.0,iotype="out")
    
    def execute(self): 
        x = self.x_y
        w1 = self.w1
        w3 = self.w3
        
        self.y1 = 0.08*x**3-0.05*w1+0.11*w3-0.09*(x*w1)**3
        self.y2 = 0.59*w3+0.41*w1**2+0.99*(x*w3)**3
        self.g_y = -226.7 + 0.26*x**3 + 0.51*w1**2 + 0.53*(x*w3)

class Z(Component): 
    
    x_z = Float(0.0,iotype="in")
    y2  = Float(0.0,iotupe="in")
    w2 = Float(0.0,iotype="in")
    
    z1 = Float(0.0,iotype="out")
    g_z = Float(0.0,iotype="out")
    
    def execute(self): 
        x = self.x_z
        y = self.y2
        w2 = self.w2
        
        self.z1 = -0.43*x**2-0.88*w2**2+0.25*(w2*x)**2
        self.g_z = -1095.1 + 0.33*y**2 + 0.47*(x*w2)
        
        
class HulmeProblem(OptProblem): 
    
    def __init__(self): 
        super(HulmeProblem,self).__init__()
        
        self.add('w',W())
        self.add('y',Y())
        self.add('z',Z())
        
        self.add_parameter('w.x_w',low=-9999,high=9999)
        self.add_parameter('y.x_y',low=-9999,high=9999)
        self.add_parameter('z.x_z',low=-9999,high=9999)
        
        self.add_objective('0.04*w.x_w**2+0.96*y.x_y**3+0.015*z.x_z-0.26*w.w1**2'
                           '+0.44*w.w2+0.57*w.w3**3-0.07*y.y1+0.68*y.y2**2'
                           '-0.02*z.z1**3',name='obj1')
        
        self.add_constraint('w.g_w <= 0')
        self.add_constraint('y.g_y <= 0')
        self.add_constraint('z.g_z <= 0')
        
        self.add_coupling_var(('w.y2','y.y2'),name='y2')
        self.add_coupling_var(('w.z1','z.z1'),name='z1')
        
        self.add_coupling_var(('y.w1','w.w1'),name='w1')
        self.add_coupling_var(('y.w3','w.w3'),name='w3')
        
        self.add_coupling_var(('z.w2','w.w2'),name='w2')
        
        
        

