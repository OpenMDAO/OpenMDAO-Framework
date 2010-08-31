import os.path
import shutil
import subprocess
import sys
import time

from openmdao.main.component import Component
from openmdao.main.container import get_default_name
from openmdao.util.log import LOG_DEBUG
from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.util.testutil import find_python

__all__ = ('check_save_load',)


def check_save_load(comp, py_dir=None, test_dir='test_dir', cleanup=True,
                    fmt=SAVE_CPICKLE, logfile=None):
    """Convenience routine to check that saving & reloading `comp` works.

    comp : Component
        The component to check.

    py_dir : string or None
        The directory in which to find local Python modules.

    test_dir : string
        Name of a scratch directory to unpack in.

    cleanup : bool
        If True, the scratch directory will be removed after the test.

    fmt : int
        The format for the saved state file.

    logfile : string or None
        Name of file for logging progress.

    Creates an egg in the current directory, unpacks it in `test_dir`
    via a separate process, and then loads and runs the component in
    another subprocess.  Returns the first non-zero subprocess exit code,
    or zero if everything succeeded.
    """
    assert isinstance(comp, Component)

    old_level = comp.log_level
    comp.log_level = LOG_DEBUG
    name = comp.name or get_default_name(comp, comp.parent)
    start = time.time()
    egg_info = comp.save_to_egg(name, 'CSL.1', py_dir=py_dir, fmt=fmt)
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
    
    try:
        print '\nUnpacking %s in subprocess...' % egg_name
        env = os.environ
        env['OPENMDAO_INSTALL'] = '0'
        if logfile:
            stdout = open(logfile, 'w')
        else:
            stdout = None
        stderr = subprocess.STDOUT

        python = find_python()
        print '    python:', python
        unpacker = 'unpack.py'
        out = open(unpacker, 'w')
        out.write("""\
from openmdao.main.api import Component
Component.load_from_eggfile(r'%s')
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
        os.chdir(orig_dir)
        comp.log_level = old_level
        if cleanup:
            os.remove(egg_name)
            shutil.rmtree(test_dir)

    return retcode

