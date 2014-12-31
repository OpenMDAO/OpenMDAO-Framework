# cd to directory of conda build script
# Create an openmdao environment and
# install numpy and scipy
# Should move all possible openmdao dependencies
# up here to simplify script
conda create --name openmdao \
  numpy \
  scipy \
  setuptools \
  pyparsing \
  traits==4.3.0 \
  nose \
  sphinx \
  fabric==0.9.3 \
  boto \
  paramiko==1.7.7.1 \
  requests \
  decorator \
  mock \
  networkx==1.8.1 \
  zope.interface \
  "pytz>=2011" \
  pycrypto==2.3 \
  cobyla==1.0.2 \
  conmin==1.0.2 \
  newsumt==1.1.1 \
  slsqp==1.0.2 \
  bson \
  pyevolve \

# Get the root directory of anaconda
source activate openmdao
PYTHON=`python -c "import sys; print sys.executable"`
source deactivate

# install openmdao packages
cd openmdao.units
$PYTHON setup.py develop
cd ../openmdao.util
$PYTHON setup.py develop
cd ../openmdao.test
$PYTHON setup.py develop
cd ../openmdao.devtools
$PYTHON setup.py develop
cd ../openmdao.main
$PYTHON setup.py develop
cd ../openmdao.lib
$PYTHON setup.py develop
# install openmdao examples
cd ../examples
cd openmdao.examples.bar3simulation
$PYTHON setup.py develop
cd ../openmdao.examples.expected_improvement
$PYTHON setup.py develop
cd ../openmdao.examples.mdao
$PYTHON setup.py develop
cd ../openmdao.examples.metamodel_tutorial
$PYTHON setup.py develop
cd ../openmdao.examples.nozzle_geometry_doe
$PYTHON setup.py develop
cd ../openmdao.examples.simple
$PYTHON setup.py develop
