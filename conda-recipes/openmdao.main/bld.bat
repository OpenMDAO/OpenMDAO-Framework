IF NOT EXIST %RECIPE_DIR%\..\..\docs\_build\html (
    openmdao build_docs --version %PKG_VERSION%
)

xcopy %RECIPE_DIR%\..\..\docs\_build\html %SRC_DIR%\%PKG_NAME%\src\openmdao\main\docs /s /e
easy_install -N %PKG_NAME%
