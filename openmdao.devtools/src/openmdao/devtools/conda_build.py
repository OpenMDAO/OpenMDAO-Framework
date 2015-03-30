import argparse
import glob
import json
import os
import shlex
import shutil
import subprocess
import sys
import tarfile
import tempfile

def create_env(name, pkgs, channel=None, yes=False):
    cmd = 'conda create --name {name}'.format(name=name)

    if channel:
        cmd = '{cmd} --channel {channel}'.format(cmd=cmd, channel=channel)

    if yes:
        cmd = '{cmd} --yes'.format(cmd=cmd)

    cmd = '{cmd} {pkgs}'.format(cmd=cmd, pkgs=' '.join(pkgs))

    subprocess.check_call(cmd.split(' '))

def list_envs():
    cmd = 'conda env list --json'

    return json.loads(subprocess.check_output(cmd.split(' ')))

def remove_env(name, yes=False):
    cmd = 'conda env remove --name {name}'.format(name=name)

    if yes:
        cmd = '{cmd} --yes'.format(cmd=cmd)

    subprocess.check_call(cmd.split(' '))

def env_exists(env_name, envs):
    for env in envs['envs']:
        if os.path.basename(env) == env_name:
            return True

    return False

def get_env_path(env_name, envs):
    for env in envs['envs']:
        if os.path.basename(env) == env_name:
            return env

    return None

def build_recipe(recipe):
    cmd = 'conda build {recipe}'.format(recipe= recipe)
    subprocess.check_call(shlex.split(cmd))

def _get_python_path(env):
    if sys.platform == 'win32':
        path = '{path}/python'.format(path=env)
    else:
        path = '{path}/bin/python'.format(path=env)

    return path

def _get_pip_path(env):
    if sys.platform == 'win32':
        path = '{path}/Scripts/pip'.format(path=env)
    else:
        path = '{path}/bin/pip'.format(path=env)

    return path

def pip_install(env, pkgs):
    cmd = '{pip} install {pkgs}'.format(
        pip=_get_pip_path(env),
        pkgs=' '.join(pkgs)
    )

    subprocess.check_call(cmd.split(' '))

def python_develop(env, pkg_path):
    cmd = '{python_path} setup.py develop --no-deps'.format(
        python_path=_get_python_path(env)
    )

    subprocess.check_call(cmd.split(' '), cwd=pkg_path)

def main():
    args = parser.parse_args()
    args.func(args)

def build_release(args):
    conda_recipes_root = os.path.join(
        root,
        'conda-recipes'
    )

    for pkg in openmdao.keys():
        recipe_path = os.path.join(
            conda_recipes_root,
            pkg
        )

        build_recipe(recipe_path)


def build_bundle(args):
    version = args.version
    temp_dir = tempfile.mkdtemp()
    start_dir = os.getcwd()

    try:
        os.putenv('CONDA_ENVS_PATH', temp_dir)

        # 1. Install OpenMDAO to a temporary conda environment
        # 2. Grab all packages
        # 3. Make tar file
        create_env(
            'openmdao-bundle',
            ['openmdao=={version}'.format(version=version)],
            channel='http://conda.binstar.org/openmdao',
            yes=True
        )

        os.chdir('{envs_path}/.pkgs'.format(envs_path=temp_dir))
        pkgs = glob.glob('*.tar.bz2')
        out = tarfile.open('openmdao.tar', mode='w')

        with tarfile.open('openmdao.tar', mode='w') as tar:
            for pkg in pkgs:
                tar.add(pkg, recursive=False)

        shutil.move(
            'openmdao.tar',
            '{start_dir}/openmdao.tar'.format(start_dir=start_dir)
        )

    finally:
        os.chdir(start_dir)
        os.unsetenv('CONDA_ENVS_PATH')
        shutil.rmtree(temp_dir)

def build_dev(args):
    env_name = args.env
    force = args.force

    # Remove environment if --force is True
    if force and env_exists(env_name, list_envs()):
        remove_env(env_name, yes=True)

    # Create conda environment
    create_env(env_name, pkgs, channel='http://conda.binstar.org/openmdao', yes=True)

    envs = list_envs()

    # use pip to install virtualenv because conda can't install version 1.9.1
    pip_install(get_env_path(env_name, envs), ['virtualenv==1.9.1'])

    # Prior steps to correctly build bar3simulation
    pkg_path = openmdao['openmdao.examples.bar3simulation']
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

    # Install all OpenMDAO packages using `python setup.py develop`
    for pkg_path in openmdao.values():
        python_develop(
            get_env_path(env_name, envs),
            os.path.join(root, pkg_path)
        )

    msg = "\nTo activate the environment, use the following command:\n\n\t {cmd} {env}\n"

    if sys.platform == 'win32':
        print msg.format(cmd='activate', env=env_name)

    else:
        print msg.format(cmd='source activate', env=env_name)

    msg = "To deactivate the environment, use the following command:\n\n\t {cmd}\n"

    if sys.platform == 'win32':
        print msg.format(cmd='deactivate')

    else:
        print msg.format(cmd='source deactivate')


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
    'openmdao.units' : 'openmdao.units',
    'openmdao.util' : 'openmdao.util',
    'openmdao.test' : 'openmdao.test',
    'openmdao.devtools' : 'openmdao.devtools',
    'openmdao.main' : 'openmdao.main',
    'openmdao.lib' : 'openmdao.lib',
    'openmdao.examples.bar3simulation' : 'examples/openmdao.examples.bar3simulation',
    'openmdao.examples.expected_improvement' : 'examples/openmdao.examples.expected_improvement',
    'openmdao.examples.mdao' : 'examples/openmdao.examples.mdao',
    'openmdao.examples.metamodel_tutorial' : 'examples/openmdao.examples.metamodel_tutorial',
    'openmdao.examples.nozzle_geometry_doe' : 'examples/openmdao.examples.nozzle_geometry_doe',
    'openmdao.examples.simple' : 'examples/openmdao.examples.simple',
}

parser = argparse.ArgumentParser(description="Process some arguments to build.py")
sub_parsers = parser.add_subparsers()
dev_parser = sub_parsers.add_parser('dev', help='help for building dev version')
dev_parser.add_argument('--env', type=str, default='openmdao-dev', help='name of environment')
dev_parser.add_argument('--force', default=False, action='store_true', help="force environment to be rebuilt if it already exists")
dev_parser.set_defaults(func=build_dev)

try:
    from openmdao.main.releaseinfo import __version__
    version = __version__
except ImportError as error:
    cmd = 'python -c "import releaseinfo; print releaseinfo.__version__"'
    cwd = os.path.join(
        root,
        'openmdao.main',
        'src',
        'openmdao',
        'main'
    )

    version = subprocess.check_output(shlex.split(cmd), cwd=cwd)

bundle_parser = sub_parsers.add_parser('bundle', help='build conda package that includes OpenMDAO and all dependencies')
bundle_parser.add_argument('-v', '--version', type=str, default=version, help="version of OpenMDAO to bundle")
bundle_parser.set_defaults(func=build_bundle)

release_parser = sub_parsers.add_parser('release', help='build conda release packages for OpenMDAO')
release_parser.set_defaults(func=build_release)

if __name__ == "__main__":
    main()
