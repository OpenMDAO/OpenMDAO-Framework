REM `python setup.py install` creates an environment
REM that doesn't copy `<anaconda root>\libs\libpython.a` to the build directory.
REM This causes link issues since the linker flags are set to 
REM `<anaconda root>\envs\_build\libs` and 
REM `<anaconda root>\envs\_build\PCbuild`
REM Using easy_install sets the linker flags to `<anaconda root>\libs\`
REM and `<anaconda root>\PCbuild` which results in a successful build, but
REM failed tests.
%PYTHON% setup.py install
if errorlevel 1 exit 1

:: Add more build steps here, if they are necessary.

:: See
:: http://docs.continuum.io/conda/build.html
:: for a list of environment variables that are set during the build process.
