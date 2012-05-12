"""
Two discipline Vane Airflow Meter. Considers structural and aerodynamic discip.ines

J. Allison, "Optimal partitioning and coordination decisions in decomposition-based design optimization",
Univeristy of Michigan, 2008.

"""

from math import cos

from openmdao.main.api import Component, VariableTree, Slot, SequentialWorkflow, Assembly
from openmdao.main.problem_formulation import OptProblem
from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.drivers.api import BroydenSolver
from openmdao.lib.components.api import ExecComp


class Constants(VariableTree): 
    k = Float(0.05, units="N/rad",desc="tortional spring constant for return spring")
    v = Float(40.0, units="m/s", desc="air flow velocity")
    C = Float(1.00, units="kg/m**3", dec="constant accounting for air density and drag coefficient")
    F_max = Float(7.00, units="N", desc="maximum allowed drag force")
    A = Float(0.1001, units="m**2", desc="required total flap area")
    theta_hat = Float(0.250, units="rad", desc="target deflection value for stator flap")

class Structural(Component): 
    
    constants = Slot(Constants(), iotype="in")
    
    l = Float(0.02,units="m", iotype="in", desc="length of the stator flap")
    F = Float(2.0,units="N", iotype="in", desc="Aerodynamic force on the stator flap")
    
    theta = Float(0.1,units="rad", iotype="in", desc="Radial deflection of the stator flap") #state variable
    
    theta_residual = Float(0.0,units="rad", iotype="out", desc="residual for the theta state variable")
    
    def execute(self): 
        
        self.theta_residual = .5*self.F*self.l*cos(self.theta) - self.constants.k*self.theta #eqn 3.1
        
class StructuralAsmb(Assembly): 
    
    def configure(self): 
        self.add('struct_comp',Structural())
        self.add('theta_passthrough',ExecComp(['theta_out=theta_in'])) #a comp to make theta an output of the asmb
        self.create_passthrough('theta_passthrough.theta_out',alias="theta")
        self.create_passthrough('struct_comp.l')
        self.create_passthrough('struct_comp.F')
        self.create_passthrough('struct_comp.theta_residual')
        self.create_passthrough('struct_comp.constants')
        
        self.add('driver',BroydenSolver())
        self.driver.add_parameter(['struct_comp.theta','theta_passthrough.theta_in'],low=0,high=1)
        self.driver.add_constraint('struct_comp.theta_residual=0')
        self.driver.workflow.add(['struct_comp','theta_passthrough'])
        
        
class Aerodynamics(Component): 
        
    constants = Slot(Constants(), iotype="in")
    l = Float(0.02,units="m", iotype="in", desc="length of the stator flap")
    w = Float(0.02,units="m", iotype="in", desc="width of the stator flap")
    theta = Float(0.1,units="rad", iotype="in", desc="Radial deflection of the stator flap") #state variable
    
    F = Float(0.0,units="N", iotype="out", desc="Aerodynamic force on the stator flap")
    
    def execute(self): 
        self.F = self.constants.C*self.l*self.w*cos(self.theta)*self.constants.v**2
            
    
class VaneAirFlow(OptProblem): 
    constants = Slot(Constants(), iotype="in")
    
    def configure(self): 
        self.constants = Constants()
        
        self.add('struct',StructuralAsmb())
        self.add('aero',Aerodynamics())
        
        self.connect('constants',['struct.constants','aero.constants'])
        
        
        
        
if __name__ == "__main__": 
    from openmdao.lib.drivers.api import BroydenSolver, SLSQPdriver
    
    p = VaneAirFlow()
    
    p.add('driver',SLSQPdriver())
    p.driver.add_parameter(['struct.l','aero.l'], low=0, high=100)
    p.driver.add_parameter('aero.w', low=0, high=100)
    p.driver.add_objective('(aero.theta-0.25)**2')
    p.driver.add_constraint('aero.F-7 <= 0')
    p.driver.add_constraint('(aero.l*aero.w - 0.01000)**2 <= 0')
    
    p.add('mda',BroydenSolver())
    #p.mda.add_parameter(['struct.theta','aero.theta'],low=-1e99,high=1e99)
    p.mda.add_parameter('struct.F',low=-1e99,high=1e99)
    #p.mda.add_constraint('struct.theta_residual=0')
    p.mda.add_constraint('struct.F=aero.F')
    p.connect('struct.theta','aero.theta')
    
    p.driver.workflow.add('mda')
    p.mda.workflow.add(['struct','aero'])
    
    p.struct.l = p.aero.l = .03
    p.aero.w = .2
    
    p.run()
    
    print p.aero.F, p.struct.F
    print p.struct.theta_residual, p.struct.theta
    print p.aero.l, p.aero.w