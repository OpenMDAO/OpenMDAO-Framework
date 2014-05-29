from openmdao.lib.optproblems.scalable import UnitScalableProblem

from openmdao.main.datatypes.api import Array, Float

from openmdao.lib.drivers.api import BroydenSolver, SensitivityDriver, CONMINdriver, FixedPointIterator

from openmdao.main.api import Component



class DebugComp(Component): 
    
    def execute(self): 
        
        print "iteration: ",self.exec_count
        print "d0.y_out: %s, d1.y_out: %s, d2.y_out: %s"%(self.parent.d0.y_out,self.parent.d1.y_out,self.parent.d2.y_out)
    
        print 'sa0.F: %s, sa0.dF: %s'%(self.parent.sa0.F,self.parent.sa0.dF,)
        print 'sa1.F: %s, sa1.dF: %s'%(self.parent.sa1.F,self.parent.sa1.dF,)
        print 'sa2.F: %s, sa2.dF: %s'%(self.parent.sa2.F,self.parent.sa2.dF,)
        print 'ssa.F: %s, ssa.dF: %s'%(self.parent.ssa.F,self.parent.ssa.dF,)
        
        print "d0_local_des_vars: %s, [%s,%s,%s]"%(self.parent.d0_local_des_vars,
                                                   self.parent.d0.x0,
                                                   self.parent.d0.x1,
                                                   self.parent.d0.x2)
        print "d1_local_des_vars: %s"%self.parent.d1_local_des_vars
        print "d2_local_des_vars: %s"%self.parent.d2_local_des_vars
        
        print "global_des_vars: %s"%self.parent.global_des_vars
        
        print 
        print 
        print 
        #raw_input()
        
        
        
#uses the default, 3 discipline, 3 globals, 3 locals
class Scalable(UnitScalableProblem): 
    
    global_des_vars   = Array([0.,0.,0.])
    d0_local_des_vars = Array([-.333,-.333,-.333])
    d1_local_des_vars = Array([-.333,-.333,-.333])
    d2_local_des_vars = Array([-.333,-.333,-.333])
    
    offset = Float(2,iotype="in")
    factor = Float(.95,iotype="in")
    percent = Float(1,iotype="in")
    
    def __init__(self): 
        super(Scalable,self).__init__()
        
        #three components: d0,d1,d2
        
        
        obj = "d0.z0**2+d0.z1**2+d0.z2**2+d0.y_out**2+d1.y_out**2+d2.y_out**2"
        
        d0_const = "1-d0.y_out/c0 <= 0"
        d1_const = "1-d1.y_out/c1 <= 0"
        d2_const = "1-d2.y_out/c2 <= 0"
        
        #initial MDA
        mda = self.add("mda",BroydenSolver())
        mda.add_parameter("d0.y_in0",low=-1e99,high=1e99)
        mda.add_parameter("d0.y_in1",low=-1e99,high=1e99)
        mda.add_constraint("d0.y_out=d1.y_in0")
        mda.add_constraint("d0.y_out=d2.y_in0")
        
        mda.add_parameter("d1.y_in0",low=-1e99,high=1e99)
        mda.add_parameter("d1.y_in1",low=-1e99,high=1e99)
        mda.add_constraint("d1.y_out=d0.y_in0")
        mda.add_constraint("d1.y_out=d2.y_in1")
        
        mda.add_parameter("d2.y_in0",low=-1e99,high=1e99)
        mda.add_parameter("d2.y_in1",low=-1e99,high=1e99)
        mda.add_constraint("d2.y_out=d0.y_in1")
        mda.add_constraint("d2.y_out=d1.y_in1")
        
        sa0 = self.add('sa0',SensitivityDriver())
        sa0.add_parameter('d0.x0',low=-10,high=10)
        sa0.add_parameter('d0.x1',low=-10,high=10)
        sa0.add_parameter('d0.x2',low=-10,high=10)
        sa0.add_objective(obj)
        sa0.add_constraint(d0_const)
        
        sa1 = self.add('sa1',SensitivityDriver())
        sa1.add_parameter('d1.x0',low=-10,high=10)
        sa1.add_parameter('d1.x1',low=-10,high=10)
        sa1.add_parameter('d1.x2',low=-10,high=10)
        sa1.add_objective(obj)
        sa1.add_constraint(d1_const)
        
        sa2 = self.add('sa2',SensitivityDriver())
        sa2.add_parameter('d0.x0',low=-10,high=10)
        sa2.add_parameter('d0.x1',low=-10,high=10)
        sa2.add_parameter('d0.x2',low=-10,high=10)
        sa2.add_objective(obj)
        sa2.add_constraint(d2_const)
        
        ssa = self.add('ssa',SensitivityDriver()) 
        ssa.add_parameter(("d0.z0","d1.z0","d2.z0"), low = -10, high = 10)
        ssa.add_parameter(("d0.z1","d1.z1","d2.z1"), low = -10, high = 10)
        ssa.add_parameter(("d0.z2","d1.z2","d2.z2"), low = -10, high = 10)
        ssa.add_objective(obj)
        ssa.add_constraint(d0_const)
        ssa.add_constraint(d1_const)
        ssa.add_constraint(d2_const)
        
        bbopt0 = self.add('bbopt0',CONMINdriver())
        bbopt0.add_parameter('d0_local_des_vars[0]',low=-10,high=10)
        bbopt0.add_parameter('d0_local_des_vars[1]',low=-10,high=10)
        bbopt0.add_parameter('d0_local_des_vars[2]',low=-10,high=10)
        bbopt0.add_objective('sa0.F[0] + sa0.dF[0][0]*(d0_local_des_vars[0]-d0.x0)'
                             '+ sa0.dF[0][1]*(d0_local_des_vars[1]-d0.x1)'
                             '+ sa0.dF[0][2]*(d0_local_des_vars[2]-d0.x2)')
        bbopt0.add_constraint('sa0.G[0] + sa0.dG[0][0]*(d0_local_des_vars[0]-d0.x0)'
                             '+ sa0.dG[0][1]*(d0_local_des_vars[1]-d0.x1)'
                             '+ sa0.dG[0][2]*(d0_local_des_vars[2]-d0.x2) <= 0')
        bbopt0.add_constraint('(d0_local_des_vars[0]-d0.x0)<=(percent*d0.x0+.0001)*factor**(mda.exec_count-offset)')
        bbopt0.add_constraint('(d0_local_des_vars[1]-d1.x1)<=(percent*d0.x1+.0001)*factor**(mda.exec_count-offset)')
        bbopt0.add_constraint('(d0_local_des_vars[2]-d2.x2)<=(percent*d0.x2+.0001)*factor**(mda.exec_count-offset)')
        bbopt0.add_constraint('(d0_local_des_vars[0]-d0.x0)>=(-percent*d0.x0-.0001)*factor**(mda.exec_count-offset)')
        bbopt0.add_constraint('(d0_local_des_vars[1]-d1.x1)>=(-percent*d0.x1-.0001)*factor**(mda.exec_count-offset)')
        bbopt0.add_constraint('(d0_local_des_vars[2]-d2.x2)>=(-percent*d0.x2-.0001)*factor**(mda.exec_count-offset)')
        
        bbopt1 = self.add('bbopt1',CONMINdriver())
        bbopt1.add_parameter('d1_local_des_vars[0]',low=-10,high=10)
        bbopt1.add_parameter('d1_local_des_vars[1]',low=-10,high=10)
        bbopt1.add_parameter('d1_local_des_vars[2]',low=-10,high=10)
        bbopt1.add_objective('sa1.F[0] + sa1.dF[0][0]*(d1_local_des_vars[0]-d1.x0)'
                             '+ sa1.dF[0][1]*(d1_local_des_vars[1]-d1.x1)'
                             '+ sa1.dF[0][2]*(d1_local_des_vars[2]-d1.x2)')
        bbopt1.add_constraint('sa1.G[0] + sa1.dG[0][0]*(d1_local_des_vars[0]-d1.x0)'
                             '+ sa1.dG[0][1]*(d1_local_des_vars[1]-d1.x1)'
                             '+ sa1.dG[0][2]*(d1_local_des_vars[2]-d1.x2) <= 0')
        
        bbopt1.add_constraint('(d1_local_des_vars[0]-d1.x0)<=(percent*d1.x0+.0001)*factor**(mda.exec_count-offset)')
        bbopt1.add_constraint('(d1_local_des_vars[1]-d1.x1)<=(percent*d1.x1+.0001)*factor**(mda.exec_count-offset)')
        bbopt1.add_constraint('(d1_local_des_vars[2]-d1.x2)<=(percent*d1.x2+.0001)*factor**(mda.exec_count-offset)')
        bbopt1.add_constraint('(d1_local_des_vars[0]-d1.x0)>=(-percent*d1.x0-.0001)*factor**(mda.exec_count-offset)')
        bbopt1.add_constraint('(d1_local_des_vars[1]-d1.x1)>=(-percent*d1.x1-.0001)*factor**(mda.exec_count-offset)')
        bbopt1.add_constraint('(d1_local_des_vars[2]-d1.x2)>=(-percent*d1.x2-.0001)*factor**(mda.exec_count-offset)')
        
        bbopt2 = self.add('bbopt2',CONMINdriver())
        bbopt2.add_parameter('d2_local_des_vars[0]',low=-10,high=10)
        bbopt2.add_parameter('d2_local_des_vars[1]',low=-10,high=10)
        bbopt2.add_parameter('d2_local_des_vars[2]',low=-10,high=10)
        bbopt2.add_objective('sa2.F[0] + sa2.dF[0][0]*(d2_local_des_vars[0]-d2.x0)'
                             '+ sa2.dF[0][1]*(d2_local_des_vars[1]-d2.x1)'
                             '+ sa2.dF[0][2]*(d2_local_des_vars[2]-d2.x2)')
        bbopt2.add_constraint('sa2.G[0] + sa2.dG[0][0]*(d2_local_des_vars[0]-d2.x0)'
                             '+ sa2.dG[0][1]*(d2_local_des_vars[1]-d2.x1)'
                             '+ sa2.dG[0][2]*(d2_local_des_vars[2]-d2.x2) <= 0')
        
        bbopt2.add_constraint('(d2_local_des_vars[0]-d2.x0)<=(percent*d2.x0+.0001)*factor**(mda.exec_count-offset)')
        bbopt2.add_constraint('(d2_local_des_vars[1]-d2.x1)<=(percent*d2.x1+.0001)*factor**(mda.exec_count-offset)')
        bbopt2.add_constraint('(d2_local_des_vars[2]-d2.x2)<=(percent*d2.x2+.0001)*factor**(mda.exec_count-offset)')
        bbopt2.add_constraint('(d2_local_des_vars[0]-d2.x0)>=(-percent*d2.x0-.0001)*factor**(mda.exec_count-offset)')
        bbopt2.add_constraint('(d2_local_des_vars[1]-d2.x1)>=(-percent*d2.x1-.0001)*factor**(mda.exec_count-offset)')
        bbopt2.add_constraint('(d2_local_des_vars[2]-d2.x2)>=(-percent*d2.x2-.0001)*factor**(mda.exec_count-offset)')
        
        sysopt = self.add('sysopt',CONMINdriver())
        sysopt.add_parameter('global_des_vars[0]',low=-10,high=10)
        sysopt.add_parameter('global_des_vars[1]',low=-10,high=10)
        sysopt.add_parameter('global_des_vars[2]',low=-10,high=10)
        sysopt.add_objective('ssa.F[0] + ssa.dF[0][0]*(global_des_vars[0]-d0.z0)'
                             '+ ssa.dF[0][1]*(global_des_vars[1]-d0.z1)'
                             '+ ssa.dF[0][2]*(global_des_vars[2]-d0.z2)')
        sysopt.add_constraint('ssa.G[0] + ssa.dG[0][0]*(global_des_vars[0]-d0.z0)'
                             '+ ssa.dG[0][1]*(global_des_vars[1]-d0.z1)'
                             '+ ssa.dG[0][2]*(global_des_vars[2]-d0.z2) <= 0')
        sysopt.add_constraint('ssa.G[1] + ssa.dG[1][0]*(global_des_vars[0]-d0.z0)'
                             '+ ssa.dG[1][1]*(global_des_vars[1]-d0.z1)'
                             '+ ssa.dG[1][2]*(global_des_vars[2]-d0.z2) <= 0')      
        sysopt.add_constraint('ssa.G[2] + ssa.dG[2][0]*(global_des_vars[0]-d0.z0)'
                             '+ ssa.dG[2][1]*(global_des_vars[1]-d0.z1)'
                             '+ ssa.dG[2][2]*(global_des_vars[2]-d0.z2) <= 0') 
        
        sysopt.add_constraint('(global_des_vars[0]-d0.z0) >= (percent*d0.z0 +.0001)*factor**(mda.exec_count-offset)')
        sysopt.add_constraint('(global_des_vars[0]-d0.z0) <= (-percent*d0.z0 -.0001)*factor**(mda.exec_count-offset)')
        sysopt.add_constraint('(global_des_vars[1]-d0.z1) >= (percent*d0.z1 +.0001)*factor**(mda.exec_count-offset)')
        sysopt.add_constraint('(global_des_vars[1]-d0.z1) <= (-percent*d0.z1 -.0001)*factor**(mda.exec_count-offset)')
        sysopt.add_constraint('(global_des_vars[2]-d0.z2) >= (percent*d0.z2 +.0001)*factor**(mda.exec_count-offset)')
        sysopt.add_constraint('(global_des_vars[2]-d0.z2) <= (-percent*d0.z2 -.0001)*factor**(mda.exec_count-offset)')
                
        debug = self.add('debug',DebugComp())
        
        driver = self.add('driver',FixedPointIterator())
        driver.add_parameter('d0.x0',low=-1e99,high=1e99)
        driver.add_parameter('d0.x1',low=-1e99,high=1e99)
        driver.add_parameter('d0.x2',low=-1e99,high=1e99)
        driver.add_constraint('d0_local_des_vars[0]=d0.x0')
        driver.add_constraint('d0_local_des_vars[1]=d0.x1')
        driver.add_constraint('d0_local_des_vars[2]=d0.x2')
        
        driver.add_parameter('d1.x0',low=-1e99,high=1e99)
        driver.add_parameter('d1.x1',low=-1e99,high=1e99)
        driver.add_parameter('d1.x2',low=-1e99,high=1e99)
        driver.add_constraint('d1_local_des_vars[0]=d1.x0')
        driver.add_constraint('d1_local_des_vars[1]=d1.x1')
        driver.add_constraint('d1_local_des_vars[2]=d1.x2')
        
        driver.add_parameter('d2.x0',low=-1e99,high=1e99)
        driver.add_parameter('d2.x1',low=-1e99,high=1e99)
        driver.add_parameter('d2.x2',low=-1e99,high=1e99)
        driver.add_constraint('d2_local_des_vars[0]=d2.x0')
        driver.add_constraint('d2_local_des_vars[1]=d2.x1')
        driver.add_constraint('d2_local_des_vars[2]=d2.x2')
        
        driver.add_parameter(['d0.z0','d1.z0','d2.z0'],low=-1e99,high=1e99)
        driver.add_parameter(['d0.z1','d1.z1','d2.z1'],low=-1e99,high=1e99)
        driver.add_parameter(['d0.z2','d1.z2','d2.z2'],low=-1e99,high=1e99)
        driver.add_constraint('global_des_vars[0]=d0.z0')
        driver.add_constraint('global_des_vars[1]=d0.z1')
        driver.add_constraint('global_des_vars[2]=d0.z2')
        
        self.driver.workflow.add(['mda', 'sa0', 'sa1','sa2','ssa', 'bbopt0', 'bbopt1', 'bbopt2','sysopt','debug'])
               
if __name__ == "__main__": 
    
        
    asmb = Scalable()
    
    asmb.d0.z0 = asmb.d1.z0 = asmb.d2.z0 = 0
    asmb.d1.z1 = asmb.d1.z1 = asmb.d2.z1 = 0
    asmb.d2.z2 = asmb.d1.z2 = asmb.d2.z2 = 0
    
    asmb.d0.x0 = asmb.d0_local_des_vars[0] = 1
    asmb.d0.x1 = asmb.d0_local_des_vars[1] = 1
    asmb.d0.x2 = asmb.d0_local_des_vars[2] = 1
    
    asmb.d1.x0 = asmb.d1_local_des_vars[0] = 1
    asmb.d1.x1 = asmb.d1_local_des_vars[1] = 1
    asmb.d1.x2 = asmb.d1_local_des_vars[2] = 1
    
    asmb.d2.x0 = asmb.d2_local_des_vars[0] = 1
    asmb.d2.x1 = asmb.d2_local_des_vars[1] = 1
    asmb.d2.x2 = asmb.d2_local_des_vars[2] = 1
        
    
    asmb.debug.run()# just print out the initial state
    
    asmb.run()
            
        
        
        
        