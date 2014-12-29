REM cd to directory of conda build script
cd /d %~dp0
SET OLD_DIR=%CD%

REM Create an openmdao environment and
REM install numpy and scipy
REM TODO: Should move all possible openmdao dependencies up here to simplify script
conda create --yes --name openmdao numpy scipy setuptools

REM Get the root directory of anaconda
conda info --root >%TEMP%\conda-root
SET /p CONDA_ROOT=<%TEMP%\conda-root
DEL %TEMP%\conda-root

REM build and install some of the dependencies from OpenMDAO recipes
REM bson
conda install --yes --name openmdao bson

IF %ERRORLEVEL% GTR 0 (
    conda build conda-packaing\bson
    conda install --yes --name openmdao bson
)

REM pyevolve
conda install --yes --name openmdao pyevolve

IF %ERRORLEVEL% GTR 0 (
    conda build conda-packaging\pyevolve
    conda install --yes --name openmdao pyevolve
)

REM cobyla
conda install --yes --name openmdao cobyla

IF %ERRORLEVEL% GTR 0 (
    conda build conda-packaging\cobyla
    conda install --force --yes --name openmdao cobyla
)

REM conmin
conda install --yes --name openmdao conmin

IF %ERRORLEVEL% GTR 0 (
    conda build conda-packaging\conmin
    conda install --force --yes --name openmdao conmin
)

REM newsumt
conda install --yes --name openmdao newsumt

IF %ERRORLEVEL% GTR 0 (
    conda build conda-packaging\newsumt
    conda install --force --yes --name openmdao newsumt
)

REM slsqp
conda install --yes --name openmdao slsqp

IF %ERRORLEVEL% GTR 0 (
    conda build conda-packaging\slsqp
    conda install --force --yes --name openmdao slsqp
)

REM install openmdao.util dependencies
conda install --yes --name openmdao pyparsing
conda install --yes --name openmdao traits==4.3
conda install --yes --name openmdao pycrypto==2.3

REM install openmdao.test dependencies
conda install --yes --name openmdao nose

REM install openmdao.devtools dependencies
conda install --yes --name openmdao sphinx
conda install --yes --name openmdao fabric^>=0.9.3
conda install --yes --name openmdao virtualenv
conda install --yes --name openmdao boto
conda install --yes --name openmdao paramiko^>=1.7.7
conda install --yes --name openmdao requests

REM install openmdao.main dependencies
conda install --yes --name openmdao decorator
conda install --yes --name openmdao mock
conda install --yes --name openmdao networkx==1.8.1
conda install --yes --name openmdao pycrypto==2.3
conda install --yes --name openmdao pyparsing
conda install --yes --name openmdao requests
conda install --yes --name openmdao sphinx
conda install --yes --name openmdao traits==4.3.0
conda install --yes --name openmdao zope.interface

REM install openmdao.lib dependencies
conda install --yes --name openmdao pytz^>=2011

SET PYTHON=%CONDA_ROOT%\envs\openmdao\python.exe
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
%PYTHON% setup.py develop

cd ..\openmdao.examples.expected_improvement
%PYTHON% setup.py develop

cd ..\openmdao.examples.mdao
%PYTHON% setup.py develop

cd ..\openmdao.examples.metamodel_tutorial
%PYTHON% setup.py develop

cd ..\openmdao.examples.nozzle_geometry_doe
%PYTHON% setup.py develop

SET CODNA_ROOT=
SET PYTHON=
CD %OLD_DIR%
SET OLD_DIR=
