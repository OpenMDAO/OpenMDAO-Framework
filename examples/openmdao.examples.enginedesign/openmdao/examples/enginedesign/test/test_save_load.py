#
# Test valid CONMIN optimization after saving and loading.
#

import logging
import os
import sys
import pkg_resources
import shutil
import subprocess
import unittest

from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
from openmdao.main.container import get_default_name
import openmdao.util.testutil


class TestCase(unittest.TestCase):
    """ Test Save/Load of Vehicle """

    def setUp(self):
        self.model = EngineOptimization()
        self.model.tree_rooted()

    def tearDown(self):
        if self.model is not None:
            self.model.pre_delete()
            self.model = None
        
    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        self.model.driving_sim.bore = 95.
        self.model.driving_sim.spark_angle = -35.368341874
        self.model.driver.itmax = 1

        # Set local dir in case we're running in a different directory.
        py_dir = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                 'test')
        python = openmdao.util.testutil.find_python()
        name = self.model.name or get_default_name(self.model, self)
        egg_info = self.model.save_to_egg(name, '0', py_dir=py_dir)
        egg_name = egg_info[0]

        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        egg_path = os.path.join('..', egg_name)
        try:
            logging.debug('Unpacking in subprocess...')
            logging.debug('    python %s' % python)
            out = open('unpack.py', 'w')
            out.write("""\
from openmdao.main.api import Component
try:
    Component.load_from_eggfile('%s')
except Exception, err:
    import openmdao.util.log
    openmdao.util.log.logger.exception(str(err))
    raise
""" % egg_path)
            out.close()
            retcode = subprocess.call([python, 'unpack.py'])
            self.assertEqual(retcode, 0)

            logging.debug('Load state and run test in subprocess...')
            logging.debug('    python %s' % python)

            os.chdir(name)
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
        self.assertAlmostEqual(model.driving_sim.accel_time, 
                               5.5999999999999961, places=6)
        self.assertAlmostEqual(model.driving_sim.EPA_city, 
                               25.203, places=3)
        self.assertAlmostEqual(model.driving_sim.EPA_highway, 
                               32.8139, places=4)
                              
if __name__ == '__main__':
    unittest.main()
""" % name)
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
 
