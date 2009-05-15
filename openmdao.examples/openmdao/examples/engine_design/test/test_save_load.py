#
# Test valid CONMIN optimization after saving and loading.
#

import logging
import os
import shutil
import subprocess
import unittest

from openmdao.examples.engine_design.engine_optimization \
    import Engine_Optimization


class EngineOptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Engine_Optimization("Test_Vehicle")

    def tearDown(self):
        if self.model is not None:
            self.model.pre_delete()
            self.model = None
        
    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        self.model.vehicle_sim.bore = 100
        self.model.vehicle_sim.sparkAngle = -35.368341874
        self.model.driver.maxiters = 1

        egg_name = self.model.save_to_egg()

        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        egg_path = os.path.join('..', egg_name)
        try:
            # Find what is hopefully the correct 'python' command.
            python = 'python'
            if orig_dir.endswith('buildout'):
                python = os.path.join(orig_dir, 'bin', python)
            else:
                index = orig_dir.find('openmdao.examples')
                if index > 0:
                    python = os.path.join(orig_dir[:index],
                                          'buildout', 'bin', python)

            logging.debug('Unpacking in subprocess...')
            logging.debug('    python %s' % python)
            out = open('unpack.py', 'w')
            out.write("""\
from openmdao.main import Component
Component.load_from_egg('%s', install=False)
""" % egg_path)
            out.close()
            retcode = subprocess.call([python, 'unpack.py'])
            self.assertEqual(retcode, 0)

            logging.debug('Load state and run test in subprocess...')
            logging.debug('    python %s' % python)

            out = open('test.py', 'w')
            out.write("""\
import sys
if not '.' in sys.path:
    sys.path.append('.')
import unittest
class TestCase(unittest.TestCase):
    def test_load(self):
        loader = __import__('%s_loader')
        model = loader.load()
        model.run()
        self.assertAlmostEqual(model.vehicle_sim.AccelTime, 
                               5.9, places=6)
        self.assertAlmostEqual(model.vehicle_sim.EPACity, 
                               25.18837, places=4)
        self.assertAlmostEqual(model.vehicle_sim.EPAHighway, 
                               30.91469, places=4)
if __name__ == '__main__':
    unittest.main()
""" % self.model.name)
            out.close()

            out = open('test.out', 'w')
            retcode = subprocess.call([python, 'test.py'],
                                      stdout=out, stderr=subprocess.STDOUT)
            out.close()
            inp = open('test.out', 'r')
            for line in inp.readlines():
                logging.debug(line.rstrip())
            inp.close()
            self.assertEqual(retcode, 0)

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)
            if egg_name and os.path.exists(egg_name):
                os.remove(egg_name)


if __name__ == '__main__':
    unittest.main()
 
