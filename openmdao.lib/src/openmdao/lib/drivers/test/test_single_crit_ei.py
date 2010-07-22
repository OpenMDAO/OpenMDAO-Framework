import unittest

from openmdao.lib.drivers.single_crit_ei import SingleCritEI
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.caserecorders.dbcaserecorder import DBCaseRecorder

from openmdao.main.api import Assembly, Case, set_as_top

from openmdao.examples.singleEI.branin_component import BraninComponent

class TestCase(unittest.TestCase): 
    
    def setUp(self): 
        
        self.top = set_as_top(Assembly())

        self.top.add("branin_meta_model",MetaModel())
        self.top.branin_meta_model.surrogate = KrigingSurrogate()
        self.top.branin_meta_model.model = BraninComponent()        
        
        self.top.add("EIdriver",SingleCritEI())
                
        self.top.DOE_trainer.add_parameter("branin_meta_model.x")
        self.top.DOE_trainer.add_parameter("branin_meta_model.y")
        self.top.DOE_trainer.add_event("branin_meta_model.train_next")
        self.top.DOE_trainer.case_outputs = ["branin_meta_model.f_xy"]        
        
        self.top.branin_meta_model.recorder = DBCaseRecorder()
        self.top.DOE_trainer.recorder = DBCaseRecorder()
        
        self.best_case = Case(outputs=[("f_xy",None,0.397887),('criteria',None,'f_xy')])
        self.bad_best_case = Case(outputs=[("f_xy",None,0.397887)])

        self.top.DOE_trainer.workflow.add(self.top.branin_meta_model)
        self.top.EIdriver.workflow.add(self.top.branin_meta_model)

        self.top.driver.workflow.add(self.top.DOE_trainer)
        self.top.driver.workflow.add(self.top.EIdriver)
        
    def tearDown(self):
        self.top = None
    
    def test_no_criteria_error(self): 
        self.top.EIdriver.best_case = ListCaseIterator([self.best_case])
        
        try:
            self.top.run()
        except ValueError,err: 
            self.assertEqual(str(err),"EIdriver: The Expression 'criteria' has not been defined")
        else: 
            self.fail("RunTimeError expected")
        
    def test_no_parameters(self):
        self.top.EIdriver.best_case = ListCaseIterator([self.best_case])
        self.top.EIdriver.criteria = "f_xy"
        try:
            self.top.run()
        except RuntimeError,err: 
            self.assertEqual(str(err),"EIdriver: no parameters were added to the driver")
        else: 
            self.fail("RuntimeError expected")
    
    def test_no_criteria_in_best_case(self): 
        
        self.top.EIdriver.best_case = ListCaseIterator([self.bad_best_case])
        self.top.EIdriver.criteria = "branin_meta_model.f_xy"
        self.top.EIdriver.add_parameter('branin_meta_model.x')
        try:
            self.top.run()
        except ValueError,err: 
            self.assertEqual(str(err),"EIdriver: best_case was not provided with a 'criteria' output, which must be present")
        else: 
            self.fail("ValueError expected")
    
    def test_norm_distrib(self):
        """Test that evaluate() returns a normal distribution"""
        pass
        
    
    def test_add_parameter(self):
        """test for correct ranges on alleles for GA"""
        self.top.EIdriver.add_parameter("branin_meta_model.x")
        self.top.EIdriver.add_parameter("branin_meta_model.y")      

        self.assertEqual(self.top.EIdriver.set_of_alleles[0][0],(-5,10))
        self.assertEqual(self.top.EIdriver.set_of_alleles[1][0],(0,15))
    
    def test_ei_prediction(self): 
        self.top.EIdriver.add_parameter("branin_meta_model.x")
        self.top.EIdriver.add_parameter("branin_meta_model.y")      

        self.top.EIdriver.best_case = ListCaseIterator([self.best_case])
        self.top.EIdriver.criteria = "branin_meta_model.f_xy"
 
        self.top.run()
        new_case = Case(inputs=[("x",None,5.0),("y",None,5.0)])
        
if __name__ == "__main__":
    unittest.main()