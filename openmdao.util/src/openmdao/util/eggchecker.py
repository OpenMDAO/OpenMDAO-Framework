import os.path
import shutil
import subprocess
import sys
import time

from openmdao.main.api import Component
from openmdao.main.log import LOG_DEBUG
from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.util.testutil import find_python


def check_save_load(comp, py_dir=None, test_dir='test_dir', cleanup=True,
                    format=SAVE_CPICKLE, logfile=None):
    """Convenience routine to check that saving & reloading work.
    It will create an egg in the current directory, unpack it in `test_dir`
    via a separate process, and then load and run the component in
    another subprocess.  Returns first non-zero subprocess exit code,
    or zero if everything succeeded.
    """
    assert isinstance(comp, Component)
    if sys.platform == 'win32':
        print '\ncheck_save_load() unsupported on win32 at this time.'
        return 0  # Enable once openmdao.util.testutil.find_python works.

    old_level = comp.log_level
    comp.log_level = LOG_DEBUG
    name = comp.name or comp.get_default_name(comp.parent)
    start = time.time()
    egg_info = comp.save_to_egg(name, 'CSL.1', py_dir=py_dir, format=format)
    egg_name = egg_info[0]
    elapsed = time.time() - start
    size = os.path.getsize(egg_name)
    print '\nSaved %d bytes in %.2f seconds (%.2f bytes/sec)' % \
          (size, elapsed, size/elapsed)

    orig_dir = os.getcwd()
    if os.path.exists(test_dir):
        shutil.rmtree(test_dir)
    os.mkdir(test_dir)
    os.chdir(test_dir)
    egg_path = os.path.join('..', egg_name)
    unpacker = None
    try:
        print '\nUnpacking %s in subprocess...' % egg_name
        env = os.environ
        env['OPENMDAO_INSTALL'] = '0'
        if logfile:
            stdout = open(logfile, 'w')
            stderr = subprocess.STDOUT
        else:
            stdout = None
            stderr = None

        python = find_python()  # Returns just 'python' if no buildout.
        print '    python:', python
        unpacker = 'unpack.py'
        out = open(unpacker, 'w')
        out.write("""\
from openmdao.main.api import Component
Component.load_from_eggfile('%s', install=False)
""" % egg_path)
        out.close()
        args = [python, unpacker]

        retcode = subprocess.call(args, env=env,
                                  stdout=stdout, stderr=stderr)
        print '    retcode', retcode
        if retcode == 0:
            print '\nRunning in subprocess...'
            os.chdir(name)
            retcode = subprocess.call([python, name+'_loader.py'],
                                      stdout=stdout, stderr=stderr)
            print '    retcode', retcode
        if logfile:
            stdout.close()
    finally:
        if unpacker and os.path.exists(unpacker):
            os.remove(unpacker)
        os.chdir(orig_dir)
        comp.log_level = old_level
        if cleanup:
            os.remove(egg_name)
            shutil.rmtree(test_dir)

    return retcode

