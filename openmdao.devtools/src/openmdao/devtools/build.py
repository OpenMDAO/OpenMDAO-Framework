import argparse
import json
import os
import shutil
import subprocess
import sys

# Path to root directory
# Should be ../../../../
root = os.path.abspath(os.path.dirname(__file__))
root = os.path.join(
    root,
    os.path.pardir,
    os.path.pardir,
    os.path.pardir,
    os.path.pardir
)

# openmdao dependencies
pkgs = [
    'pip',
    'numpy',
    'scipy',
    'setuptools',
    'pyparsing',
    'traits==4.3.0',
    'nose',
    'sphinx==1.2.2',
    'fabric==0.9.3',
    'boto',
    'paramiko==1.7.7.1',
    'requests',
    'decorator',
    'mock',
    'networkx',
    'zope.interface',
    'pytz>=2014.4',
    'pycrypto==2.3',
    'cobyla==1.0.2',
    'conmin==1.0.2',
    'slsqp==1.0.2',
    'newsumt==1.1.1',
    'bson',
    'pyevolve'
]

openmdao = {
    'units' : 'openmdao.units',
    'util' : 'openmdao.util',
    'test' : 'openmdao.test',
    'devtools' : 'openmdao.devtools',
    'main' : 'openmdao.main',
    'lib' : 'openmdao.lib',
    'bar3simulation' : 'examples/openmdao.examples.bar3simulation',
    'expected_improvement' : 'examples/openmdao.examples.expected_improvement',
    'mdao' : 'examples/openmdao.examples.mdao',
    'metamodel_tutorial' : 'examples/openmdao.examples.metamodel_tutorial',
    'nozzle_geometry_doe' : 'examples/openmdao.examples.nozzle_geometry_doe',
    'simple' : 'examples/openmdao.examples.simple',
}

# Install all openmdao dependencies into a conda environment
env_name = 'openmdao-dev'

cmd = 'conda create --name {env_name} --channel http://conda.binstar.org/openmdao --yes {pkgs}'
cmd = cmd.format(pkgs=' '.join(pkgs), env_name=env_name)

subprocess.check_call(cmd.split(' '))

envs = subprocess.check_output(['conda', 'env', 'list', '--json'])
envs = json.loads(envs)

# Get the path to the python interpreter
for env in envs['envs']:
    if os.path.basename(env) == env_name:
        if sys.platform == 'win32':
            python_path = '{path}/python'.format(path=env)
        else:
            python_path = '{path}/bin/python'.format(path=env)


# Prior steps to correctly build bar3simulation
pkg_path = openmdao['bar3simulation']
pkg_path = os.path.join(root, pkg_path)

# 1. Forcibly remove the bar3 extension if it exists
try:
    os.remove('{pkg_path}/openmdao/examples/bar3simulation/bar3.so'.format(
        pkg_path=pkg_path)
    )

except Exception as error:
    print error

# 2. Forcibly remove any build directories
try:
    shutil.rmtree('{pkg_path}/build'.format(pkg_path=pkg_path))
except Exception as error:
    print error

# 3. Forcibly remove any dist directories
try:
    shutil.rmtree('{pkg_path}/dist'.format(pkg_path=pkg_path))
except Exception as error:
    print error

for pkg_path in openmdao.values():
    pkg_path = os.path.join(root, pkg_path)
    subprocess.check_call([python_path, 'setup.py', 'develop', '--no-deps'], cwd=pkg_path)
