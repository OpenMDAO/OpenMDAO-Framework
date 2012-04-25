import unittest, os.path, shutil
import json

from openmdao.gui.consoleserver import ConsoleServer

class ConsoleServerTestCase(unittest.TestCase):
    
    def setUp(self):
        self.path = os.path.dirname(os.path.abspath(__file__))
        self.cserver = ConsoleServer()        

    def test_simple(self):
        ''' load and inspect the simple example project
        '''
        
        # LOAD PROJECT
        self.cserver.load_project(os.path.join(self.path,'simple_1.proj'))
            
        # CHECK FILES
        files = self.cserver.get_files()
        
        self.assertTrue('\paraboloid.py' in files)
        self.assertTrue('\optimization_constrained.py' in files)
        self.assertTrue('\optimization_unconstrained.py' in files)
        self.assertTrue('\_project_state' in files)
        
        # IMPORT PARABOLOID
        self.cserver.default('from paraboloid import Paraboloid')
        
        working_types = self.cserver.get_workingtypes()
        self.assertTrue('Paraboloid' in working_types)
        
        type_info = working_types['Paraboloid']
        self.assertEqual(type_info['path'], 'Paraboloid')
        self.assertEqual(type_info['version'], 'n/a')

        # CREATE ASSEMBLY
        self.cserver.add_component('prob','openmdao.main.assembly.Assembly','');
        
        components = json.loads(self.cserver.get_components())
        self.assertEqual(len(components), 1)
        
        assembly = components[0]
        self.assertEqual(assembly['pathname'], 'prob')
        self.assertEqual(assembly['type'], 'Assembly')
        self.assertEqual(assembly['interfaces'],
            ['IAssembly', 'IComponent', 'IContainer'])
        self.assertEqual(len(assembly['children']), 1)
        
        child = assembly['children'][0]
        self.assertEqual(child['pathname'], 'prob.driver')
        self.assertEqual(child['type'], 'Run_Once')
        self.assertEqual(child['interfaces'],
            ['IDriver', 'IHasEvents', 'IComponent', 'IContainer'])
        
        
        # ADD CONMIN DRIVER
        self.cserver.add_component('driver',
           'openmdao.lib.drivers.conmindriver.CONMINdriver','prob');

        components = json.loads(self.cserver.get_components())
        self.assertEqual(len(components), 1)
        
        assembly = components[0]
        self.assertEqual(assembly['pathname'], 'prob')
        self.assertEqual(len(assembly['children']), 1)
        
        child = assembly['children'][0]
        self.assertEqual(child['pathname'], 'prob.driver')
        self.assertEqual(child['type'], 'CONMINdriver')
        self.assertEqual(child['interfaces'], 
            ['IHasParameters', 'IHasIneqConstraints', 'IHasObjective',
             'IDriver', 'IHasEvents', 'IComponent', 'IContainer'])
           
        # CHECK DRIVER ATTRIBUTES
        attributes = json.loads(self.cserver.get_attributes('prob.driver'))
        self.assertEqual(attributes['type'], 'CONMINdriver')
        self.assertTrue('Inputs' in attributes)
        self.assertTrue('Outputs' in attributes)
        self.assertTrue('Objectives' in attributes)
        self.assertTrue('Parameters' in attributes)
        self.assertTrue('IneqConstraints' in attributes)
        self.assertTrue('Slots' in attributes)
        self.assertTrue('Workflow' in attributes)
        
        self.assertEqual(attributes['Workflow']['pathname'], 'prob.driver')
        self.assertEqual(attributes['Workflow']['type'], 'openmdao.lib.drivers.conmindriver.CONMINdriver')
        self.assertEqual(len(attributes['Workflow']['workflow']), 0)

        # CREATE PARABOLOID
        self.cserver.add_component('p','Paraboloid','prob')

        attributes = json.loads(self.cserver.get_attributes('prob.p'))
        self.assertEqual(attributes['type'], 'Paraboloid')
        
        self.assertTrue('Inputs' in attributes)
        inputs = attributes['Inputs']
        self.assertEqual(len(inputs), 4)
        found_x = found_y = False
        for input in inputs:
            self.assertTrue('desc'  in input)
            self.assertTrue('high'  in input)
            self.assertTrue('low'   in input)
            self.assertTrue('name'  in input)
            self.assertTrue('type'  in input)
            self.assertTrue('units' in input)
            self.assertTrue('valid' in input)
            self.assertTrue('value' in input)
            if input['name'] == 'x':
                found_x = True
                self.assertEqual(input['type'], 'float')
                self.assertEqual(input['desc'], 'The variable x')
            if input['name'] == 'y':
                found_y = True
                self.assertEqual(input['type'], 'float')
                self.assertEqual(input['desc'], 'The variable y')
        self.assertTrue(found_x)
        self.assertTrue(found_y)
        
        self.assertTrue('Outputs' in attributes)
        outputs = attributes['Outputs']
        self.assertEqual(len(outputs), 1)
        found_f_xy = False
        for output in outputs:
            self.assertTrue('desc'  in output)
            self.assertTrue('high'  in output)
            self.assertTrue('low'   in output)
            self.assertTrue('name'  in output)
            self.assertTrue('type'  in output)
            self.assertTrue('units' in output)
            self.assertTrue('valid' in output)
            self.assertTrue('value' in output)
            if output['name'] == 'f_xy':
                found_f_xy = True
                self.assertEqual(output['type'], 'float')
                self.assertEqual(output['desc'], 'F(x,y)')
        self.assertTrue(found_f_xy)
        
        # STRUCTURE
        structure = json.loads(self.cserver.get_structure('prob'))
        
        self.assertEqual(len(structure), 2)
        
        self.assertTrue('components' in structure)
        components = structure['components']
        self.assertEqual(len(components), 2)
        found_p = found_driver = False
        for comp in components:
            self.assertTrue('pathname' in comp)
            self.assertTrue('type'     in comp)
            self.assertTrue('name'     in comp)
            if comp['name'] == 'p':
                found_p = True
                self.assertEqual(comp['pathname'], 'prob.p')
                self.assertEqual(comp['type'], 'Paraboloid')
            if comp['name'] == 'driver':
                found_driver = True
                self.assertEqual(comp['pathname'], 'prob.driver')
                self.assertEqual(comp['type'], 'CONMINdriver')
        self.assertTrue(found_p)
        self.assertTrue(found_driver)

        self.assertTrue('connections' in structure)        
        connections = structure['connections']
        self.assertEqual(len(connections), 0)
        
        # WORKFLOW
        self.cserver.default('prob.driver.workflow.add("p")')
        driver_flow = json.loads(self.cserver.get_workflow('prob.driver'))
        self.assertTrue('pathname' in driver_flow)
        self.assertTrue('type'     in driver_flow)
        self.assertTrue('workflow' in driver_flow)
        workflow = driver_flow['workflow']
        self.assertEqual(len(workflow), 1)
        self.assertEqual(workflow[0]['pathname'], 'prob.p')
        self.assertEqual(workflow[0]['type'], 'paraboloid.Paraboloid')
        
        
    def test_execfile(self):
        ''' execfile an input file (with a __main__) and make sure you
            can save the project without any errors
        '''
        proj_file = os.path.join(self.path,'simple_1.proj')
        proj_copy = os.path.join(self.path,'simple_2.proj')
        shutil.copyfile(proj_file,proj_copy)
        
        self.cserver.load_project(proj_copy)
        self.cserver.execfile('optimization_constrained.py')
        self.cserver.save_project()
        
        os.remove(proj_copy)
        
        
    def tearDown(self):
        self.cserver.cleanup()

            
if __name__ == "__main__":
    unittest.main()
