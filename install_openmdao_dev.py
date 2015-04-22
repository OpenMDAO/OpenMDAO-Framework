#install_openmdao.py

#This file will act as a unified installer for OpenMDAO
#It will attempt to ascertain the user's environment
#and then install using either: Anaconda Python or regular python/Virtualenv
import sys
import subprocess

#If version of Python is unsatisfactory, throw an error with an
#admonition that python must be of a certain version (currently >2.7, <3.0)
def check_version():
    print '\nInstalling OpenMDAO...'
    if sys.version_info < (2, 7) or sys.version_info > (3, 0):
        print'ERROR: {0}'.format(sys.exc_info()[1])
        print'ERROR: you have Python {0}.{1}.{2} installed'.format(sys.version_info[0], sys.version_info[1], sys.version_info[2])
        print'ERROR: this script requires Python greater than 2.7 and less than 3.0.'
        sys.exit(101)

    else:
        print  '\nACCEPTABLE PYTHON VERSION: {0}.{1}.{2} FOUND'.format(sys.version_info[0], sys.version_info[1], sys.version_info[2])

#Find out which python (Anaconda/regular) is installed as the default Python
def check_dist():
    version = sys.version
    #Looking for either 'Anaconda' or 'Continuum Analytics' for proof of Anaconda.
    if "Analytics" in version or "Anaconda" in version:
        dist = 'Anaconda'
        print 'Anaconda detected. Running conda_build.py with args supplied.'
    else:
        dist = 'Virtualenv'
        print 'Regular Python detected. Running go-openmdao file with args supplied.'

    return dist

def install(dist):
    #call the appropriate installer with the python that ran this script.
    cmd = [sys.executable]
    if dist == "Anaconda":
        cmd.append('openmdao.devtools/src/openmdao/devtools/conda_build.py')
        cmd.append('dev')
    elif dist == "Virtualenv":
        cmd.append('go-openmdao-dev.py')

    #Pass through args in argv other than the 0th entry, which is a cmd
    for arg in sys.argv[1:]:
        cmd.append(arg)

    subprocess.call(cmd)


if __name__ == "__main__":
    check_version()
    dist = check_dist()
    install(dist)
