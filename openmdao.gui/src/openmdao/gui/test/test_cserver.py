import unittest
import os.path
import shutil
import json
import time
import tempfile

from openmdao.gui.consoleserver import ConsoleServer

from openmdao.main.publisher import Publisher
from openmdao.main.project import project_from_archive
from openmdao.util.fileutil import onerror


class ConsoleServerTestCase(unittest.TestCase):

    def setUp(self):
        self.tdir = tempfile.mkdtemp()
        self.path = os.path.dirname(os.path.abspath(__file__))
        self.cserver = ConsoleServer()
        Publisher.silent = True  # keep quiet about Publisher not being set up

    def tearDown(self):
        self.cserver.cleanup()
        try:
            shutil.rmtree(self.tdir, onerror=onerror)
        except:
            pass

    def test_simple(self):
        ''' load and inspect the simple example project
        '''

        projfile = os.path.join(self.path, 'simple_1.proj')

        project_from_archive(projfile, dest_dir=self.tdir)

        # LOAD PROJECT
        self.cserver.load_project(os.path.join(self.tdir, 'simple_1'))

        # CHECK FILES
        files = self.cserver.get_files()

        self.assertTrue('/paraboloid.py'.replace('/', os.sep) in files)
        self.assertTrue('/optimization_constrained.py'.replace('/', os.sep) in files)
        self.assertTrue('/optimization_unconstrained.py'.replace('/', os.sep) in files)
        self.assertTrue('/_project_state'.replace('/', os.sep) in files)

        time.sleep(3.0)
        types = self.cserver.get_types()
        self.assertTrue('Paraboloid' in types)

        type_info = types['Paraboloid']
        self.assertEqual(type_info['modpath'], 'paraboloid.Paraboloid')

        components = json.loads(self.cserver.get_components())

        # CREATE ASSEMBLY
        self.cserver.put_object('prob', 'openmdao.main.assembly.Assembly')

        oldnum = len(components)
        components = json.loads(self.cserver.get_components())
        self.assertEqual(len(components) - oldnum, 1)

        for comp in components:
            if comp['pathname'] == 'prob':
                assembly = comp
                break
        else:
            self.fail("prob was not found in component list")
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
        self.cserver.put_object('prob.driver',
            'openmdao.lib.drivers.conmindriver.CONMINdriver')

        components = json.loads(self.cserver.get_components())
        self.assertEqual(len(components) - oldnum, 1)

        for comp in components:
            if comp['pathname'] == 'prob':
                assembly = comp
                break
        else:
            self.fail("prob was not found in component list")
        self.assertEqual(assembly['pathname'], 'prob')
        self.assertEqual(len(assembly['children']), 1)

        child = assembly['children'][0]
        self.assertEqual(child['pathname'], 'prob.driver')
        self.assertEqual(child['type'], 'CONMINdriver')
        self.assertEqual(child['interfaces'],
            ['IHasParameters', 'IHasIneqConstraints', 'IHasObjective',
             'IOptimizer', 'IDriver', 'IHasEvents', 'IComponent', 'IContainer'])

        # CHECK DRIVER ATTRIBUTES
        attributes = json.loads(self.cserver.get_attributes('prob.driver'))
        self.assertEqual(attributes['type'], 'CONMINdriver')
        self.assertTrue('Inputs' in attributes)
        self.assertTrue('Outputs' in attributes)
        self.assertTrue('Objectives' in attributes)
        self.assertTrue('Parameters' in attributes)
        self.assertTrue('Constraints' in attributes)
        self.assertTrue('Slots' in attributes)
        self.assertTrue('Workflow' in attributes)

        self.assertEqual(attributes['Workflow']['pathname'],
                        'prob.driver')
        self.assertEqual(attributes['Workflow']['type'],
                        'openmdao.lib.drivers.conmindriver.CONMINdriver')
        self.assertEqual(len(attributes['Workflow']['workflow']), 0)

        self.assertEqual(self.cserver.file_forces_reload('/paraboloid.py'), False)

        # CREATE PARABOLOID
        self.cserver.put_object('prob.p', 'paraboloid.Paraboloid')

        self.assertEqual(self.cserver.file_forces_reload('/paraboloid.py'), True)

        attributes = json.loads(self.cserver.get_attributes('prob.p'))
        self.assertEqual(attributes['type'], 'Paraboloid')

        self.assertTrue('Inputs' in attributes)
        inputs = attributes['Inputs']
        self.assertEqual(len(inputs), 4)
        found_x = found_y = False
        for item in inputs:
            self.assertTrue('desc' in item)
            self.assertTrue('name' in item)
            self.assertTrue('type' in item)
            self.assertTrue('valid' in item)
            self.assertTrue('value' in item)
            if item['name'] == 'x':
                found_x = True
                self.assertEqual(item['type'], 'float')
                self.assertEqual(item['desc'], 'The variable x')
            if item['name'] == 'y':
                found_y = True
                self.assertEqual(item['type'], 'float')
                self.assertEqual(item['desc'], 'The variable y')
        self.assertTrue(found_x)
        self.assertTrue(found_y)

        self.assertTrue('Outputs' in attributes)
        outputs = attributes['Outputs']
        self.assertEqual(len(outputs), 4)
        found_f_xy = False
        for output in outputs:
            self.assertTrue('desc' in output)
            self.assertTrue('name' in output)
            self.assertTrue('type' in output)
            self.assertTrue('valid' in output)
            self.assertTrue('value' in output)
            if output['name'] == 'f_xy':
                found_f_xy = True
                self.assertEqual(output['type'], 'float')
                self.assertEqual(output['desc'], 'F(x,y)')
        self.assertTrue(found_f_xy)

        # DATAFLOW
        dataflow = json.loads(self.cserver.get_dataflow('prob'))

        self.assertEqual(len(dataflow), 5)

        self.assertTrue('components' in dataflow)
        components = dataflow['components']
        self.assertEqual(len(components), 2)
        found_p = found_driver = False
        for comp in components:
            self.assertTrue('pathname' in comp)
            self.assertTrue('type' in comp)
            self.assertTrue('name' in comp)
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

        self.assertTrue('connections' in dataflow)
        connections = dataflow['connections']
        self.assertEqual(len(connections), 0)

        self.assertTrue('parameters' in dataflow)
        parameters = dataflow['parameters']
        self.assertEqual(len(parameters), 0)

        self.assertTrue('constraints' in dataflow)
        constraints = dataflow['constraints']
        self.assertEqual(len(constraints), 0)

        self.assertTrue('objectives' in dataflow)
        objectives = dataflow['objectives']
        self.assertEqual(len(objectives), 0)

        # WORKFLOW
        self.cserver.onecmd('prob.driver.workflow.add("p")')
        driver_flow = json.loads(self.cserver.get_workflow('prob.driver'))[0]
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
        proj_file = os.path.join(self.path, 'simple_1.proj')
        proj_copy = os.path.join(self.tdir, 'simple_2.proj')
        shutil.copyfile(proj_file, proj_copy)
        project_from_archive(proj_copy)

        self.cserver.load_project(os.path.join(self.tdir, 'simple_2'))
        self.cserver.execfile('optimization_constrained.py')
        self.cserver.commit_project()

        self.cserver.cleanup()
        os.remove(proj_copy)


if __name__ == "__main__":
    unittest.main()
