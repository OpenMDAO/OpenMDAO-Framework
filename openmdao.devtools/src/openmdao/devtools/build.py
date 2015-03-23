import argparse
import json
import os
import shutil
import subprocess
import sys

def env_exists(env_name):
    envs = subprocess.check_output(['conda', 'env', 'list', '--json'])
    envs = json.loads(envs)

    for env in envs['envs']:
        if os.path.basename(env) == env_name:
            return True

    return False


def build_dev(args):
    env_name = args.env
    force = args.force

    # Install all openmdao dependencies into a conda environment
    if force and env_exists(env_name):
        cmd = 'conda remove --name {env_name} --all --yes'
        cmd = cmd.format(env_name=env_name)

        subprocess.check_call(cmd.split(' '))
        
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
                pip_path = '{path}/Scripts/pip'.format(path=env)
            else:
                python_path = '{path}/bin/python'.format(path=env)
                pip_path = '{path}/bin/pip'.format(path=env)

    subprocess.check_call([pip_path, 'install', 'virtualenv==1.9.1'])

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
    'pyevolve',
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

parser = argparse.ArgumentParser(description="Process some arguments to build.py")
sub_parsers = parser.add_subparsers()
dev_parser = sub_parsers.add_parser('dev', help='help for building dev version')
dev_parser.add_argument('--env', type=str, default='openmdao-dev', help='name of environment')
dev_parser.add_argument('--force', default=False, action='store_true', help="force environment to be rebuilt if it already exists")
dev_parser.set_defaults(func=build_dev)


if __name__ == "__main__":
    args = parser.parse_args()
    args.func(args)
