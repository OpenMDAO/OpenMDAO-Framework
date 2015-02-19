if [ ! -d $RECIPE_DIR/../../docs/_build/html ]; then
    openmdao build_docs
fi

cp -R $RECIPE_DIR/../../docs/_build/html $SRC_DIR/$PKG_NAME/src/openmdao/main/docs

easy_install -N $PKG_NAME

