"""
Speed Reducer Test Problem

Tosserams, S, L F P Etman, and J E Rooda. “An augmented Lagrangian decomposition 
method for quasi-separable problems in MDO.” Structural and Multidisciplinary 
Optimization 34.3 (2006) : 211-227.
""" 

from openmdao.main.api import Component, OptProblem
from openmdao.lib.datatypes.api import Float

C1 = 0.7854
C2 = 3.3333
C3 = 14.9335
C4 = 43.0934
C5 = 1.5907
C6 = 7.477
C7 = 40.0
C8 = 12.0
C9 = 3.6 
C10 = 27.0
C11 = 397.5
C12 = 5.0
C13 = 2.6
C14 = 3.9
C15 = 1.5
C16 = 1.9
C17 = 1.93
C18 = 110
C19 = 0.1
C20 = 1.69e7
C21 = 745.0
C22 = 2.9
C23 = 5.5
C24 = 1.1
C25 = 1.93
C26 = 85
C27 = 1.575e8
C28 = 5.0


class Gear(Component): 
    
    x1 = Float(0.0,iotype="in")
    x2 = Float(0.0 iotype="in")
    x3 = Float(0.0 iotype="in")
    
    f1 = Float(0.0,iotype="out")
    
    g5 = Float(0.0,iotype="out")
    g6 = Float(0.0,iotype="out")
    g9 = Float(0.0,iotype="out")
    g10 = Float(0.0,iotype="out")
    g11 = Float(0.0,iotype="out")
    
    def execute(self): 
        x1 = self.x1 
        x2 = self.x2
        x3 = self.x3 
        
        self.f1 = C1 * x1 * x2**2 * (C2*x3**2 + C3*x3 - C4)
        
        self.g5 = C10/(x1* x2**2 * x3) - 1 
        self.g6 = C11/(x1* x2**2 * x3**2) - 1 
        self.g9 = x2*x3/C7 - 1
        self.g10 = 5*x2/x1 - 1
        self.g11 = x1/(C8*x2) -1 
        
        
class Shaft1(Component): 
    
    x1 = Float(0.0,iotype="in")
    x2 = Float(0.0,iotype="in")
    x3 = Float(0.0,iotype="in")
    x4 = Float(0.0,iotype="in")
    x6 = Float(0.0,iotype="in")
    
    f2 = Float(0.0,iotype="out")
    f4 = Float(0.0,iotype="out")
    f6 = Float(0.0,iotype="out")
    
    g1 = Float(0.0,iotype="out")
    g3 = Float(0.0,iotype="out")
    g7 = Float(0.0,iotype="out")
    
    
    def execute(self): 
        x1 = self.x1
        x2 = self.x2
        x3 = self.x3
        x4 = self.x4
        x6 = self.x5
        
        self.f2 = -C5*x1*x6**2
        self.f4 = C6*x6**3
        self.f6 = C1*x4*x6**2
        
        self.g1 = 1/(C18*x6**3)*((C21*x4/(x2*x3))**2 + C20)**.5 - 1
        self.g3 = (C15*x6+C16)/x4 - 1
        self.g7 = (C17*x4**3)/(x2*x3*x6**4) - 1
        
        
class Shaft2(Component): 
    
    x1 = Float(0.0,iotype="in")
    x2 = Float(0.0,iotype="in")
    x3 = Float(0.0,iotype="in")
    x5 = Float(0.0,iotype="in")
    x7 = Float(0.0,iotype="in")
    
    f3 = Float(0.0,iotype="out")
    f5 = Float(0.0,iotype="out")
    f7 = Float(0.0,iotype="out")
    
    g2 = Float(0.0,iotype="out")
    g4 = Float(0.0,iotype="out")
    g8 = Float(0.0,iotype="out")
        
       
    def execute(self): 
        
        x1 = self.x1
        x2 = self.x2
        x3 = self.x3 
        x5 = self.x5
        x7 = self.x7
        
        self.f3 = -C5*x1*x7**2
        self.f5 = C6*x7**3
        self.f7 = C1*x5*x7**2
        
        self.g2 = 1/(C26*x7**3)*((C21*x5/(x2*x3))**2 + C27)**.5 - 1
        self.g4 = (C24*x7 + C16)/x4 - 1
        self.g8 = (C25*x5**3)/(x2*x3*x7**4) - 1
        
        
        
class SpeedReducerProblem(OptProblem): 
    
    def __init__(self): 
        super(SpeedReducerProblem,self).__init__()
        
        self.add('gear',Gear())
        self.add('shaft1',Shaft1())
        self.add('shaft2',Shaft2())
        
        #Globals
        self.add_parameter(('gear.x1','shaft1.x1','shaft2.x1'),low=2.6,high=3.6,start=3.4980,name='x1')
        self.add_parameter(('gear.x2','shaft1.x2','shaft2.x2'),low=0.7,high=0.8,start=0.7029,name='x2')
        self.add_parameter(('gear.x3','shaft1.x3','shaft2.x3')low=17,high=28,start=20.1022,name='x3')
        
        #Locals
        self.add_parameter('shaft1.x4',low=7.3,high=8.3,start=7.4879,name='x4')
        self.add_parameter('shaft1.x5',low=7.3,high=8.3,start=7.9088,name='x5')
        
        self.add_parameter('shaft2.x6',low=2.9,high=3.9,start=3.1094,name='x6')
        self.add_parameter('shaft2.x7',low=5.0,high=5.5,start=5.4989,name='x7')
        
        #constraints
        self.add_constraint('gear.g5 <= 0')
        self.add_constraint('gear.g6 <= 0')
        self.add_constraint('gear.g9 <= 0')
        self.add_constraint('gear.g10 <= 0')
        self.add_constraint('gear.g11 <= 0')
        
        self.add_constraint('shaft1.g1 <= 0')
        self.add_constraint('shaft1.g3 <= 0')
        self.add_constraint('shaft1.g7 <= 0')
        
        self.add_constraint('shaft2.g2 <= 0')
        self.add_constraint('shaft2.g4 <= 0')
        self.add_constraint('shaft2.g8 <= 0')
        
        #single objective
        self.add_objective('gear.f1+shaft1.f2+shaft1.f4+shaft1.f6+'
                           'shaft2.f3+shaft2.f5+shaft2.f7', name='obj')
        
        #solution
        self.solution = {'x1':3.5,
                    'x2':0.70,
                    'x3':17.00,
                    'x4':7.30,
                    'x5':7.72,
                    'x6':3.35,
                    'x7':5.29,
                    'obj':294
                    }
