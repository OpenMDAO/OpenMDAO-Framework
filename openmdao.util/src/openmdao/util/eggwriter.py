"""
Write Python egg file, either directly or via setuptools.
Supports what's needed for saving and loading components/simulations.
"""

import os.path
import platform
import subprocess
import time
import zipfile

__all__ = ('write', 'write_via_setuptools')


def write(name, doc, version, loader, src_files, distributions,
          dst_dir, logger):
    """
    Write egg in manner of setuptools, with some differences:
    - Write directly to zip file, avoiding some intermediate copies.
    - Don't compile any Python modules.
    """
    py_version = platform.python_version_tuple()
    py_version = '%s.%s' % (py_version[0], py_version[1])
    egg_name = '%s-%s-py%s.egg' % (name, version, py_version)
    egg_path = os.path.join(dst_dir, egg_name)
    sources = set()
    egg_info = 'EGG-INFO'

    # Eggsecutable support.
    logger.debug('Creating %s', egg_path)
    egg = open(egg_path, 'w')
    egg.write("""\
#!/bin/sh
if [ `basename $0` = "%(egg_name)s" ]
then exec python%(py_version)s -c "import sys, os; sys.path.insert(0, os.path.abspath('$0')); from %(name)s.%(loader)s import eggsecutable; sys.exit(eggsecutable())" "$@"
else
  echo $0 is not the correct name for this egg file.
  echo Please rename it back to %(egg_name)s and try again.
  exec false
fi
""" % {'egg_name':egg_name, 'py_version':py_version,
       'name':name, 'loader':loader})
    egg.close()

    egg = zipfile.ZipFile(egg_path, 'a', zipfile.ZIP_DEFLATED)

    # Write src_files.
    for path in sorted(src_files):
        path = os.path.join(name, path)
        logger.debug("    adding '%s'", path)
        egg.write(path)

    # Write Python modules.
    # TODO: use 2.6 followlinks.
    for dirpath, dirnames, filenames in os.walk('.'):
        for path in sorted(filenames):
            if path.endswith('.py'):
                if dirpath != '.':
                    path = os.path.join(dirpath, path)
                logger.debug("    adding '%s'", path)
                egg.write(path)
                sources.add(path+'\n')

    for dirpath, dirnames, filenames in os.walk(name):
        for path in sorted(filenames):
            if path.endswith('.py'):
                path = os.path.join(dirpath, path)
                logger.debug("    adding '%s'", path)
                egg.write(path)
                sources.add(path+'\n')

    # Package info -> EGG-INFO/PKG-INFO
    egg.writestr(os.path.join(egg_info, 'PKG-INFO'), """\
Metadata-Version: 1.0
Name: %(name)s
Version: %(version)s
Summary: %(doc)s
Home-page: UNKNOWN
Author: UNKNOWN
Author-email: UNKNOWN
License: UNKNOWN
Description: UNKNOWN
Platform: UNKNOWN
""" % {'name':name.replace('_', '-'), 'version':version, 'doc':doc.strip()})
    sources.add(name+'.egg-info/PKG-INFO\n')

    # Dependency links -> EGG-INFO/dependency_links.txt
    egg.writestr(os.path.join(egg_info, 'dependency_links.txt'), '\n')
    sources.add(name+'.egg-info/dependency_links.txt\n')

    # Entry points -> EGG-INFO/entry_points.txt
    egg.writestr(os.path.join(egg_info, 'entry_points.txt'), """\
[openmdao.components]
%(name)s = %(loader)s:load

[openmdao.top]
top = %(loader)s:load

[setuptools.installation]
eggsecutable = %(name)s.%(loader)s:eggsecutable

""" % {'name':name, 'loader':loader})
    sources.add(name+'.egg-info/entry_points.txt\n')

    # Unsafe -> EGG-INFO/not-zip-safe
    egg.writestr(os.path.join(egg_info, 'not-zip-safe'), '\n')
    sources.add(name+'.egg-info/not-zip-safe\n')

    # Requirements -> EGG-INFO/requires.txt
    requirements = ''
    for dist in sorted(distributions, key=lambda dist: dist.project_name):
        requirements += '%s == %s\n' % (dist.project_name, dist.version)
    egg.writestr(os.path.join(egg_info, 'requires.txt'), requirements)
    sources.add(name+'.egg-info/requires.txt\n')

    # Top-level names -> EGG-INFO/top_level.txt
    egg.writestr(os.path.join(egg_info, 'top_level.txt'), '%s\n' % name)
    sources.add(name+'.egg-info/top_level.txt\n')

    # Manifest -> EGG-INFO/SOURCES.txt
    sources.add(name+'.egg-info/SOURCES.txt\n')
    egg.writestr(os.path.join(egg_info, 'SOURCES.txt'),
                 ''.join(sorted(sources)))

    egg.close()


def write_via_setuptools(name, doc, version, loader, src_files, distributions,
                         dst_dir, logger):
    """ Write an egg via setuptools. """ 
    _write_setup_py(name, doc, version, loader, src_files, distributions)

    # Use environment since 'python' might not recognize '-u'.
    env = os.environ
    env['PYTHONUNBUFFERED'] = '1'
    proc = subprocess.Popen(['python', 'setup.py', 'bdist_egg',
                             '-d', dst_dir], env=env,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    output = []
    while proc.returncode is None:
        line = proc.stdout.readline()
        if line:
            line = line.rstrip()
            logger.debug('    '+line)
            output.append(line)
        time.sleep(0.1)
        proc.poll()
    line = proc.stdout.readline()
    while line:
        line = line.rstrip()
        logger.debug('    '+line)
        output.append(line)
        line = proc.stdout.readline()

    if proc.returncode != 0:
        for line in output:
            logger.error('    '+line)
        logger.error('save_to_egg failed due to setup.py error %d:',
                     proc.returncode)
        raise RuntimeError('setup.py failed, check log for info.')


def _write_setup_py(name, doc, version, loader, src_files, distributions):
    """ Write setup.py file for installation later. """
    out = open('setup.py', 'w')
    
    out.write('import setuptools\n')

    out.write('\npackage_files = [\n')
    for filename in sorted(src_files):
        path = os.path.join(name, filename)
        if not os.path.exists(path):
            raise ValueError("Can't save, '%s' does not exist" % path)
        out.write("    '%s',\n" % filename)
    out.write(']\n')
    
    out.write('\nrequirements = [\n')
    for dist in distributions:
        out.write("    '%s == %s',\n" % (dist.project_name, dist.version))
    out.write(']\n')
    
    out.write("""
entry_points = {
    'openmdao.top' : [
        'top = %(loader)s:load',
    ],
    'openmdao.components' : [
        '%(name)s = %(loader)s:load',
    ],
    'setuptools.installation' : [
        'eggsecutable = %(name)s.%(loader)s:eggsecutable',
    ],
}

setuptools.setup(
    name='%(name)s',
    description='''%(doc)s''',
    version='%(version)s',
    packages=setuptools.find_packages(),
    package_data={'%(name)s' : package_files},
    zip_safe=False,
    install_requires=requirements,
    entry_points=entry_points,
)
""" % {'name':name, 'loader':loader, 'doc':doc.strip(), 'version':version})

    out.close()

