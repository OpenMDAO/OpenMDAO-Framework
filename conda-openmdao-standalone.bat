@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION

REM Get dummy environment name
SET DUMMY_ENV=%~1

IF [%DUMMY_ENV%] == [] (
    ECHO "Must provide an environment name
    EXIT /B 1
)

REM Create an environment for the openmdao metapackage
conda create -y --name %DUMMY_ENV% openmdao==0.12.0

REM get root environment
conda info --root > %TEMP%\conda-root
SET /p CONDA_ROOT=<%TEMP%\conda-root
DEL %TEMP%\conda-root

REM get package cache
SET CONDA_PKG_CACHE=%CONDA_ROOT%\pkgs

REM Generate a list of all packages in the environment
SET PKGS=

FOR /F %%A IN ('conda list --name %DUMMY_ENV% --canonical') DO (
    SET PKGS=!PKGS! %CONDA_PKG_CACHE%\%%A.tar.bz2
)

REM Create a tar file
7z a -ttar -so openmdao %PKGS% | 7z a -si openmdao.tar.gz

REM Rename `openmdao.tar.gz` to `openmdao.tar`
REM because Anaconda doesn't recognize
REM the `tar.gz` extension
rename openmdao.tar.gz openmdao.tar

REM CLEANUP
conda env remove -y --name %DUMMY_ENV%

ENDLOCAL
@ECHO ON
