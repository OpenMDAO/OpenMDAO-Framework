"""Heart Dipole Problem taken from http://www.eng.buffalo.edu/Research/MODEL/test_problem_4.html"""

from openmdao.main.api import Component
from openmdao.main.problem_formulation import OptProblem

from openmdao.lib.datatypes.api import Float

#dipole momements for the problem
d_mx = 0.63254
d_my = -1.34534
d_a = -0.8365348 
d_b = 1.7345334
d_c = 1.352352
d_d = -0.843453
d_e = -0.9563453 
d_f = 1.2342523

class Dipole1(Component): 
    
    x1 = Float(0.0,iotype="in")
    x4 = Float(0.0,iotype="in")
    x6 = Float(0.0,iotype="in")
    x7 = Float(0.0,iotype="in")

    x3 = Float(0.0,iotype="in")
    x5 = Float(0.0,iotype="in")

    x2 = Float(0.0,iotype="out")
    x8 = Float(0.0,iotype="out")
    
    f5 = Float(0.0,iotype="out")
    f7 = Float(0.0,iotype="out")
    
    def execute(self): 
        x1 = self.x1
        x4 = self.x4        
        x6 = self.x6
        x7 = self.x7
        
        x3 = self.x3
        x5 = self.x5
                
        #f1 = 0 = x1 + x2 - d_mx
        self.x2 = x2 = d_mx - x1
        
        #f3 = 0 = x1*x8 + x6*x2 - x7*x3 - x8*x4 - d_a
        self.x8 = x8 = (x5*x1 + x6*x2 - x7*x3 - d_a)/x4
        
        self.f5 = x1*(x5**2-x7**2) - 2*x3*x5*x7 + x2*(x6**2-x8**2) - 2*x4*x6*x8 - - d_c
        self.f7 = x1*x5*(x5**2-3*x7**2) + x3*x7*(x7**2-3*x5**2) \
                  + x2*x6*(x6**2-3*x8**2) + x4*x8*(x8**2-3*x6**2) - d_e
        
class Dipole2(Component): 
    
    x1 = Float(0.0,iotype="in")
    x4 = Float(0.0,iotype="in")
    x6 = Float(0.0,iotype="in")
    x7 = Float(0.0,iotype="in")

    x2 = Float(0.0,iotype="in")
    x8 = Float(0.0,iotype="in")

    x3 = Float(0.0,iotype="out")
    x5 = Float(0.0,iotype="out")
    
    f6 = Float(0.0,iotype="out")
    f8 = Float(0.0,iotype="out")
    
    def execute(self): 
        x1 = self.x1
        x4 = self.x4        
        x6 = self.x6
        x7 = self.x7
        
        x2 = self.x2
        x8 = self.x8
                
        #f2 = 0 = x3 + x4 - d_my
        self.x3 = x3 = d_my - x4
        
        
        #f4 = 0 = x7*x1 + x8*x2 - x5*x3 - x6*x4 - d_b
        self.x5 = x5 = (d_b - x7*x1 - x8*x2 - x6*x4 )/x3
        
        self.f6 = x3*(x5**2-x7**2) + 2*x1*x5*x7 + x4*(x6**2-x8**2) + 2*x2*x6*x8 - d_d
        self.f8 = x3*x5*(x5**2-3*x7**2) - x1*x7*(x7**2-3*x5**2) + x4*x6*(x6**2-3*x8**2) \
            + x2*x8*(x8**2-3*x6**2) - d_f
        
        #print "f6: %f, f8: %f"%(self.f6,self.f8)
                
               
class HeartDipoleProblem(OptProblem): 
    
    def __init__(self): 
        super(HeartDipoleProblem,self).__init__()
        
        self.add('d1',Dipole1())
        self.add('d2',Dipole2())
        
        #Globals
        self.add_parameter(('d1.x1','d2.x1'),low=0,high=400,start=4,name='x1')
        self.add_parameter(('d1.x4','d2.x4'),low=0,high=400,start=3,name='x4')
        self.add_parameter(('d1.x6','d2.x6'),low=0,high=400,start=2,name='x6')
        self.add_parameter(('d1.x7','d2.x7'),low=0,high=400,start=1,name='x7')
        
        self.add_constraint('d1.f5 >= 0')
        self.add_constraint('d1.f7 >= 0')
        self.add_constraint('d2.f6 >= 0')
        self.add_constraint('d2.f8 >= 0')
        
        self.add_coupling_var(('d1.x3','d2.x3'),name="x3",start=1)
        self.add_coupling_var(('d1.x5','d2.x5'),name="x5",start=1)
        self.add_coupling_var(('d2.x2','d1.x2'),name="x2",start=1)
        self.add_coupling_var(('d2.x8','d1.x8'),name="x8",start=1)
        
        self.add_objective('d1.f5+d1.f7+d2.f6+d2.f8')
        
        
        
        
        