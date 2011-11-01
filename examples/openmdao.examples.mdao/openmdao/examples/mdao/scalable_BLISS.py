from openmdao.lib.optproblems.scalable import UnitScalableProblem

from openmdao.lib.datatypes.api import Array

from openmdao.lib.drivers.api import BroydenSolver, SensitivityDriver, CONMINdriver

from openmdao.lib.differentiators.finite_difference import FiniteDifference

from openmdao.main.api import Component



class DebugComp(Component): 
    
    def execute(self): 
        print "d0.y_out: %s, d1.y_out: %s, d2.y_out: %s"%(self.parent.d0.y_out,self.parent.d1.y_out,self.parent.d2.y_out)
    
        print 'sa0.F: %s, sa0.dF: %s'%(self.parent.sa0.F,self.parent.sa0.dF,)
        print 'sa1.F: %s, sa1.dF: %s'%(self.parent.sa1.F,self.parent.sa1.dF,)
        print 'sa2.F: %s, sa2.dF: %s'%(self.parent.sa2.F,self.parent.sa2.dF,)
        print 'ssa.F: %s, ssa.dF: %s'%(self.parent.ssa.F,self.parent.ssa.dF,)
        
        print "d0_local_des_vars: %s"%self.parent.d0_local_des_vars
        print "d1_local_des_vars: %s"%self.parent.d1_local_des_vars
        print "d2_local_des_vars: %s"%self.parent.d2_local_des_vars
        
        print "global_des_vars: %s"%self.parent.global_des_vars
        
        print 
        print 
        print 
        
        
        
#uses the default, 3 discipline, 3 globals, 3 locals
class Scalable(UnitScalableProblem): 
    
    global_des_vars   = Array([0.,0.,0.])
    d0_local_des_vars = Array([-.333,-.333,-.333])
    d1_local_des_vars = Array([-.333,-.333,-.333])
    d2_local_des_vars = Array([-.333,-.333,-.333])
    
    def __init__(self): 
        super(Scalable,self).__init__()
        
        #three components: d0,d1,d2
        
        obj = "d0.z0**2+d0.z1**2+d0.z2**2+d0.y_out**2+d1.y_out**2+d2.y_out**2"
        
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

        mda.force_execute = True
        
        sa0 = self.add('sa0',SensitivityDriver())
        sa0.differentiator = FiniteDifference()
        sa0.add_parameter('d0.x0',low=-10,high=10)
        sa0.add_parameter('d0.x1',low=-10,high=10)
        sa0.add_parameter('d0.x2',low=-10,high=10)
        sa0.add_objective(obj)
        
        sa1 = self.add('sa1',SensitivityDriver())
        sa1.differentiator = FiniteDifference()
        sa1.add_parameter('d1.x0',low=-10,high=10)
        sa1.add_parameter('d1.x1',low=-10,high=10)
        sa1.add_parameter('d1.x2',low=-10,high=10)
        sa1.add_objective(obj)
        
        sa2 = self.add('sa2',SensitivityDriver())
        sa2.differentiator = FiniteDifference()
        sa2.add_parameter('d0.x0',low=-10,high=10)
        sa2.add_parameter('d0.x1',low=-10,high=10)
        sa2.add_parameter('d0.x2',low=-10,high=10)
        sa2.add_objective(obj)
        
        ssa = self.add('ssa',SensitivityDriver()) 
        ssa.differentiator = FiniteDifference()
        ssa.add_parameter(("d0.z0","d1.z0","d2.z0"), low = -10, high = 10)
        ssa.add_parameter(("d0.z1","d1.z1","d2.z1"), low = -10, high = 10)
        ssa.add_parameter(("d0.z2","d1.z2","d2.z2"), low = -10, high = 10)
        ssa.add_objective(obj)
        
        bbopt0 = self.add('bbopt0',CONMINdriver())
        bbopt0.add_parameter('d0_local_des_vars[0]',low=-10,high=10)
        bbopt0.add_parameter('d0_local_des_vars[1]',low=-10,high=10)
        bbopt0.add_parameter('d0_local_des_vars[2]',low=-10,high=10)
        bbopt0.add_objective('sa0.F[0] + sa0.dF[0][0]*(d0_local_des_vars[0]-d0.x0)'
                             '+ sa0.dF[0][1]*(d0_local_des_vars[1]-d0.x1)'
                             '+ sa0.dF[0][2]*(d0_local_des_vars[2]-d0.x2)')
        
        bbopt1 = self.add('bbopt1',CONMINdriver())
        bbopt1.add_parameter('d1_local_des_vars[0]',low=-10,high=10)
        bbopt1.add_parameter('d1_local_des_vars[1]',low=-10,high=10)
        bbopt1.add_parameter('d1_local_des_vars[2]',low=-10,high=10)
        bbopt1.add_objective('sa1.F[0] + sa1.dF[0][0]*(d1_local_des_vars[0]-d1.x0)'
                             '+ sa1.dF[0][1]*(d1_local_des_vars[1]-d1.x1)'
                             '+ sa1.dF[0][2]*(d1_local_des_vars[2]-d1.x2)')
        
        bbopt2 = self.add('bbopt2',CONMINdriver())
        bbopt2.add_parameter('d2_local_des_vars[0]',low=-10,high=10)
        bbopt2.add_parameter('d2_local_des_vars[1]',low=-10,high=10)
        bbopt2.add_parameter('d2_local_des_vars[2]',low=-10,high=10)
        bbopt2.add_objective('sa2.F[0] + sa2.dF[0][0]*(d2_local_des_vars[0]-d2.x0)'
                             '+ sa2.dF[0][1]*(d2_local_des_vars[1]-d2.x1)'
                             '+ sa2.dF[0][2]*(d2_local_des_vars[2]-d2.x2)')
        
        
        sysopt = self.add('sysopt',CONMINdriver())
        sysopt.add_parameter('global_des_vars[0]',low=-10,high=10)
        sysopt.add_parameter('global_des_vars[1]',low=-10,high=10)
        sysopt.add_parameter('global_des_vars[2]',low=-10,high=10)
        sysopt.add_objective('ssa.F[0] + ssa.dF[0][0]*(global_des_vars[0]-d0.z0)'
                             '+ ssa.dF[0][1]*(global_des_vars[1]-d0.z1)'
                             '+ ssa.dF[0][2]*(global_des_vars[2]-d0.z2)')
        
        
        debug = self.add('debug',DebugComp())
        debug.force_execute = True
        
        self.driver.workflow.add(['mda', 'sa0', 'sa1','sa2','ssa', 'bbopt0', 'bbopt1', 'bbopt2','sysopt','debug'])
               
if __name__ == "__main__": 
    
        
    asmb = Scalable()
    
    asmb.d0.z0 = asmb.d1.z0 = asmb.d2.z0 = 0
    asmb.d1.z1 = asmb.d1.z1 = asmb.d2.z1 = 0
    asmb.d2.z2 = asmb.d1.z2 = asmb.d2.z2 = 0
    
    asmb.d0.x0 = asmb.d0_local_des_vars[0] = -.333
    asmb.d0.x1 = asmb.d0_local_des_vars[1] =-.333
    asmb.d0.x2 = asmb.d0_local_des_vars[2] =-.333
    
    asmb.d1.x0 = asmb.d1_local_des_vars[0] =-.333
    asmb.d1.x1 = asmb.d1_local_des_vars[1] =-.333
    asmb.d1.x2 = asmb.d1_local_des_vars[2] =-.333
    
    asmb.d2.x0 = asmb.d2_local_des_vars[0] =-.333
    asmb.d2.x1 = asmb.d2_local_des_vars[1] =-.333
    asmb.d2.x2 = asmb.d2_local_des_vars[2] =-.333
        
    
    asmb.debug.run()# just print out the initial state
    
    asmb.run()
            
        
        
        
        