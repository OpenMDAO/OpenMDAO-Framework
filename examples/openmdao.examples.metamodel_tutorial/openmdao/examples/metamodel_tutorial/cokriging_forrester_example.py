"""
Cokriging example from [Forrester 2007] to show
MultiFiMetaModel and MultiFiCoKrigingSurrogate usage
"""

import numpy as np

from openmdao.main.api import implements, Container, Driver
from openmdao.main.api import Assembly, Component, SequentialWorkflow

from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.drivers.api import CaseIteratorDriver
from openmdao.lib.components.api import MultiFiMetaModel
from openmdao.lib.surrogatemodels.api import MultiFiCoKrigingSurrogate, KrigingSurrogate

class Model(Component):
    x   = Float(0, iotype="in")
    f_x = Float(0.0, iotype="out")
    
    def execute(self):
        x = self.x
        self.f_x = ((6*x-2)**2)*np.sin((6*x-2)*2)
    
            
class LowFidelityModel(Component): 
    x   = Float(0.0, iotype="in")
    f_x = Float(0.0, iotype="out")
    
    def execute(self):
        x = self.x
        self.f_x = 0.5*((6*x-2)**2)*np.sin((6*x-2)*2)+(x-0.5)*10. - 5


class HighFidelityModel(Model): 
    pass

         
class CasesBuilder(Assembly):        

    def __init__(self, model, cases):
        self.instance = model    
        self.cases = cases
        super(CasesBuilder, self).__init__()
    
    def configure(self):  
        self.add("model", self.instance)
        self.add("driver", CaseIteratorDriver())
        
        self.driver.workflow.add('model')
        self.driver.add_parameter("model.x", low=0, high=1)
        self.driver.add_response("model.f_x")
        self.driver.case_inputs.model.x = self.cases 
        
        self.create_passthrough('driver.case_inputs.model.x')
        self.create_passthrough('driver.case_outputs.model.f_x')
        

class Simulation(Assembly):

    def __init__(self, surrogate, nfi=1):
        self.surrogate = surrogate
        self.nfi = nfi
        super(Simulation, self).__init__()
    
    def configure(self):
        
        # Expensive and Cheap DOE (note: have to be nested)
        doe_e = [0.0, 0.4, 0.6, 1.0]
        doe_c = [0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9] + doe_e 
        self.add('hifi_cases', CasesBuilder(HighFidelityModel(), doe_e))
        self.add('lofi_cases', CasesBuilder(LowFidelityModel(), doe_c))
        
        # MetaModel
        self.add("meta_model", MultiFiMetaModel(params = ('x', ), 
                                                responses = ('f_x', ), nfi=self.nfi)) 
        self.meta_model.default_surrogate = self.surrogate
        self.connect('hifi_cases.x'  , 'meta_model.params.x')
        self.connect('hifi_cases.f_x', 'meta_model.responses.f_x')
        if self.nfi>1:
            self.connect('lofi_cases.x'  , 'meta_model.params.x_fi2')
            self.connect('lofi_cases.f_x', 'meta_model.responses.f_x_fi2')
          
        # Iteration Hierarchy
        self.driver.workflow = SequentialWorkflow()
        
        self.add('mm_trainer', Driver())
        self.mm_trainer.workflow.add('hifi_cases')
        if self.nfi>1:
            self.mm_trainer.workflow.add('lofi_cases')
            
        self.add('mm_checker', CaseIteratorDriver())
        self.add('model', Model())
        self.mm_checker.add_parameter("meta_model.x", low=0, high=1)
        self.mm_checker.add_parameter("model.x", low=0, high=1)
        self.mm_checker.add_response("model.f_x")
        self.mm_checker.add_response("meta_model.f_x")
        ngrid = 100
        self.mm_checker.case_inputs.meta_model.x = np.linspace(0,1,ngrid)
        self.mm_checker.case_inputs.model.x = np.linspace(0,1,ngrid)
        
        self.driver.workflow.add(['mm_trainer', 'mm_checker'])
    
if __name__ == "__main__":
            
    surrogate = MultiFiCoKrigingSurrogate()
    
    # Co-kriging with 2 levels of fidelity    
    sim_cok = Simulation(surrogate, nfi=2)    
    sim_cok.run()
        
    predicted_cok = np.array([d.mu for d in sim_cok.mm_checker.case_outputs.meta_model.f_x])
    sigma_cok = np.array([d.sigma for d in sim_cok.mm_checker.case_outputs.meta_model.f_x])
    
    # Co-kriging with 1 level of fidelity a.k.a. kriging   
    # surrogate = KrigingSurrogate()   # uncomment to use the existing Kriging implementation
    sim_k = Simulation(surrogate, nfi=1) 
    sim_k.run()

    predicted_k = np.array([d.mu for d in sim_k.mm_checker.case_outputs.meta_model.f_x])
    sigma_k = np.array([d.sigma for d in sim_k.mm_checker.case_outputs.meta_model.f_x])
    
    actual = sim_k.mm_checker.case_outputs.model.f_x
    check  = sim_k.mm_checker.case_inputs.meta_model.x   
        
    import pylab as p
    
    p.figure(2)
    p.ioff()
    p.plot(check, actual, 'k', label='True f')        
    p.plot(sim_cok.hifi_cases.x, sim_cok.hifi_cases.f_x,'ok',label="High Fi")
    p.plot(sim_cok.lofi_cases.x, sim_cok.lofi_cases.f_x,'or',label="Low Fi")
    p.plot(check, predicted_cok, 'g', label='Co-kriging')
    p.plot(check, predicted_cok + 2*sigma_cok, 'g', alpha=0.5, label='I95%')
    p.plot(check, predicted_cok - 2*sigma_cok, 'g', alpha=0.5)
    p.fill_between(check, predicted_cok + 2*sigma_cok,
                          predicted_cok - 2*sigma_cok, facecolor='g', alpha=0.2)
    p.plot(check, predicted_k, 'b', label='Krigring')
    p.plot(check, predicted_k + 2*sigma_k, 'b', alpha=0.5, label='I95%')
    p.plot(check, predicted_k - 2*sigma_k, 'b', alpha=0.5)
    p.fill_between(check, predicted_k + 2*sigma_k,
                          predicted_k - 2*sigma_k, facecolor='b', alpha=0.2)
            
    p.legend(loc='best')
    p.show()
    
        
    # RMSE CoKriging
    erreur=0.
    for a,p in zip(actual,predicted_cok): 
        erreur+=(a-p)**2        
    erreur=(erreur/len(actual))
    print "RMSE Cokriging = %g" %erreur  
    
    # RMSE Kriging
    erreur=0.
    for a,p in zip(actual, predicted_k): 
        erreur+=(a-p)**2
    erreur=(erreur/len(actual))
    print "RMSE Kriging = %g" % erreur  