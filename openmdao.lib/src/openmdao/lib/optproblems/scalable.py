from openmdao.main.api import Component
from openmdao.main.problem_formulation import OptProblem

from openmdao.lib.datatypes.api import Float


class Discipline(Component): 
    
    y_out = Float(0.0,iotype="out",desc="outputs state variable")
    c_y_out = Float(1.0,iotype="in",
                    desc="coefficient for the output state variable")
    
    def __init__(self,n_global=1,n_local=1,n_coupling=1): 
        
        super(Discipline,self).__init__()        
        
        self.global_vars = []
        
        for i in range(0,n_global): 
            self.add_trait("z%d"%i,Float(0.0,
                                     iotype="in",
                                     desc="global variable #%d"%i))
            
            self.add_trait('c_z%d'%i,Float(1.0,
                                     iotype="in",
                                     desc="global variable coefficient #%d"%i))
            
            self.global_vars.append(('c_z%d'%i,"z%d"%i))
            
        
            self.local_vars=[]
            
        for i in range(0,n_local): 
            self.add_trait("x%d"%i,Float(0.0,
                                     iotype="in",
                                     desc="local variable #%d"%i))
            
            self.add_trait('c_x%d'%i,Float(1.0,
                                     iotype="in",
                                     desc="local variable coefficient #%d"%i))
            
            self.local_vars.append(("c_x%d"%i,"x%d"%i))
            
        self.coupling_vars = []
            
        for i in range(0,n_coupling): 
            self.add_trait("y_in%d"%i,Float(0.01,
                                     iotype="in",
                                     desc="coupling variable #%d"%i))
            
            self.add_trait('c_y_in%d'%i,Float(1.0,
                                     iotype="in",
                                     desc="coupling variable coefficient #%d"%i))
            
            self.coupling_vars.append(('c_y_in%d'%i,"y_in%d"%i))
    
            
    def execute(self):     
        cz = sum([getattr(self,c)*getattr(self,z) for c,z in self.global_vars])
        cx = sum([getattr(self,c)*getattr(self,x) for c,x in self.local_vars])
        cy = sum([-getattr(self,c)*getattr(self,y) for c,y in self.coupling_vars])
        self.y_out = -1/self.c_y_out*(cz+cx+cy)
                
        
class UnitScalableProblem(OptProblem):         
    def __init__(self,n_disciplines=3,n_globals=3,n_locals=3): 
        super(UnitScalableProblem,self).__init__()
        
        self.solution = {}
        
        self.disciplines = []
        for i in range(0,n_disciplines): 
            name = 'd%d'%i
            #each discipline as n_discipline-1 coupling vars
            d = self.add(name,Discipline(n_globals,n_locals,n_disciplines-1))
            self.disciplines.append(name)
            d.c_y_out = n_disciplines #scale to the number of disciplines to keep values of y at 1
            
            #scale roughly to the magnitude 
            #self.add_trait("c%d"%i,Float(1.0,iotype="in",desc="scaler for the constraint on the %dth discipline"%i))
            
            #adding all local variables to the problem formulation
            for j in range(0,n_locals): 
                self.add_parameter('%s.x%d'%(name,j),low=-10,high=10,start=0)
                self.solution['%s.x%d'%(name,j)] = -1/float(n_locals)
        
            #self.add_constraint("1-%s.y_out/2.0 <= 0"%name) #if you wanted to scale the constraint use this line
            self.add_constraint("1-%s.y_out <= 0"%name)
            
        
        #global variables
        for i in range(0,n_globals): 
            params = tuple(["%s.z%d"%(name,i) for name in self.disciplines])
            self.add_parameter(params,low=-10,high=10,start=0, name="z%d"%i)
            self.solution["z%d"%i] = 0
            
        #coupling vars 
        for d in self.disciplines: 
            i = 0
            for other_d in self.disciplines: 
                if not d == other_d:
                    self.add_coupling_var(("%s.y_in%d"%(d,i),"%s.y_out"%other_d),start = 0)
                    self.solution[("%s.y_in%d"%(d,i),"%s.y_out"%other_d)] = 1.0
                    i+=1
        #objective
        parts = []
        for d in self.disciplines:
            for i in range(0,n_globals):      
                parts.append("%s.z%d**2"%(d,i)) 
            parts.append("%s.y_out**2"%d)
            
         
        self.add_objective("+".join(parts),name="obj1")
        self.solution['obj1'] = float(n_disciplines)        
        
        
        
    
if __name__ == '__main__': 
    from openmdao.lib.architectures.api import MDF, CO, BLISS
    
    sp = UnitScalableProblem(3,3,3)
    
    sp.architecture = MDF()
    
    sp.run()
    
    error = sp.check_solution()
    
    
    for k,v in sp.solution.iteritems(): 
        print k,v
    print 
    #print sp.d0.x0    
    for k,v in error.iteritems(): 
        print k,v
        
        
    #print sum([v**2 for k,v in error.iteritems()])**.5
    
        
        