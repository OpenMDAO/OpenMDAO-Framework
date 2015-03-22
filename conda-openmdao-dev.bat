@ECHO OFF
REM cd to directory of conda build script
cd /d %~dp0
SET OLD_DIR=%CD%

REM Create an openmdao environment and
REM install numpy and scipy
REM TODO: Should move all possible openmdao dependencies up here to simplify script
SET ENV_NAME=%~1

IF [%ENV_NAME%] == [] SET ENV_NAME=openmdao

SET DEPENDENCIES=pip
SET DEPENDENCIES=%DEPENDENCIES% numpy
SET DEPENDENCIES=%DEPENDENCIES% scipy
SET DEPENDENCIES=%DEPENDENCIES% setuptools
SET DEPENDENCIES=%DEPENDENCIES% pyparsing
SET DEPENDENCIES=%DEPENDENCIES% traits==4.3.0
SET DEPENDENCIES=%DEPENDENCIES% nose
SET DEPENDENCIES=%DEPENDENCIES% sphinx==1.2.2
SET DEPENDENCIES=%DEPENDENCIES% "fabric==0.9.3"
SET DEPENDENCIES=%DEPENDENCIES% virtualenv
SET DEPENDENCIES=%DEPENDENCIES% boto
SET DEPENDENCIES=%DEPENDENCIES% paramiko==1.7.7.1
SET DEPENDENCIES=%DEPENDENCIES% requests
SET DEPENDENCIES=%DEPENDENCIES% decorator
SET DEPENDENCIES=%DEPENDENCIES% mock
SET DEPENDENCIES=%DEPENDENCIES% networkx
SET DEPENDENCIES=%DEPENDENCIES% zope.interface
SET DEPENDENCIES=%DEPENDENCIES% "pytz>=2014.4"
SET DEPENDENCIES=%DEPENDENCIES% pycrypto==2.3
SET DEPENDENCIES=%DEPENDENCIES% cobyla
SET DEPENDENCIES=%DEPENDENCIES% conmin
SET DEPENDENCIES=%DEPENDENCIES% newsumt
SET DEPENDENCIES=%DEPENDENCIES% slsqp
SET DEPENDENCIES=%DEPENDENCIES% bson
SET DEPENDENCIES=%DEPENDENCIES% pyevolve

@ECHO ON

conda create --name %ENV_NAME% %DEPENDENCIES%

@ECHO OFF
SET DEPENDENCIES=

REM Get the root directory of anaconda
cmd /c "activate %ENV_NAME% && python -c "import sys; print sys.executable" > %TEMP%\python && deactivate"
SET /p PYTHON=<%TEMP%\python
DEL %TEMP%\python

REM install openmdao packages
cd openmdao.units
%PYTHON% setup.py develop
cd ..\openmdao.util
%PYTHON% setup.py develop
cd ..\openmdao.test
%PYTHON% setup.py develop
cd ..\openmdao.devtools
%PYTHON% setup.py develop
cd ..\openmdao.main
%PYTHON% setup.py develop
cd ..\openmdao.lib
%PYTHON% setup.py develop
REM install openmdao examples
cd ..\examples
cd openmdao.examples.bar3simulation

REM Manually remove bar3.pyd since
REM `python setup.py develop` won't update it
if EXIST openmdao\examples\bar3simulation\bar3.pyd (
    del openmdao\examples\bar3simulation\bar3.pyd
)

REM Also remove build directory
if EXIST build (
    rmdir /s /q build
)

REM And remove dist directory
if EXIST dist (
    rmdir /s /q dist
)

REM Then build development version of bar3simulation
%PYTHON% setup.py develop

cd ..\openmdao.examples.expected_improvement
%PYTHON% setup.py develop
cd ..\openmdao.examples.mdao
%PYTHON% setup.py develop
cd ..\openmdao.examples.metamodel_tutorial
%PYTHON% setup.py develop
cd ..\openmdao.examples.nozzle_geometry_doe
%PYTHON% setup.py develop
cd ..\openmdao.examples.simple
%PYTHON% setup.py develop

SET PYTHON=
CD %OLD_DIR%
SET OLD_DIR=*/
@ECHO ON
