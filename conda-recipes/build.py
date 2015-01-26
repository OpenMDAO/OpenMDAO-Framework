import argparse
import sys
import os
from subprocess import Popen

def build_packages(name, version):
    packages = [
        "openmdao.units",
        "openmdao.util",
        "openmdao.test",
        "openmdao.main",
        "openmdao.lib",
        "openmdao.examples.bar3simulation",
        "openmdao.examples.expected_improvement",
        "openmdao.examples.mdao",
        "openmdao.examples.metamodel_tutorial",
        "openmdao.examples.nozzle_geometry_doe",
        "openmdao.examples.simple",
    ]
    
    package_id = {
        "all" : slice(0, len(packages), 1),
        "core" : slice(0,5,1),
        "examples" : slice(5, len(packages), 1)
    }

    env = dict(os.environ)
    env['PKG_VERSION'] = version
    
    for package in packages[package_id[name]]: 
        path = os.path.dirname(__file__)
        path = os.path.join(path, package)

        command = ['conda', 'build', path]
        process = Popen(command, env=env)
        process.wait()
    
def main(argv):
    parser = argparse.ArgumentParser(description="Build conda packages for openmdao")
    parser.add_argument('package', type=str)
    parser.add_argument('version',  type=str)
    
    namespace = parser.parse_args()
    build_packages(namespace.package, namespace.version)
    
if __name__ == "__main__":
    main(sys.argv)
