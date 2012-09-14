/**
 * stuff to do after the code editor page is loaded
 */

jQuery(function() {
    // define openmdao namespace & create interface to openmdao in global scope
    openmdao = (typeof openmdao === 'undefined' || !openmdao ) ? {} : openmdao ;
    if (opener && opener.openmdao && opener.openmdao.model ) {
        openmdao.model = opener.openmdao.model;
        openmdao.model.addWindow(window);
    }
    else {
        openmdao.model = new openmdao.Model();
    }

    var code = new openmdao.CodeFrame('code_pane', openmdao.model);

    function code_fn(path) { code.editFile(path); }
    function geom_fn(path) { openmdao.Util.popupWindow('geometry?path='+path,'Geometry'); }

    var ftree = new openmdao.FileTreeFrame('file_pane', openmdao.model, code_fn, geom_fn);

    // allow frames to close in an orderly fashion before closing window
    jQuery(window).bind('beforeunload', function(e) {
        code.close();
        ftree.close();
        openmdao.model.editor = undefined;
    });

    // set the layout (note: global scope)
    layout = jQuery('body').layout({
        onresize: function(e) {
            // resize content pane of all tabbed panes to fill the layout pane
            var layout_pane = jQuery('.ui-layout-'+e),
                pane_height = layout_pane.height(),
                pane_width  = layout_pane.width();
            jQuery(layout_pane.children('div:first').each(function() {
                var panel = jQuery(this);
                panel.height(pane_height);
                panel.width(pane_width);
            }));
            code.resize();
        }
    });

    // redo layout when window is resized
    jQuery(window).resize(function(e) {
        jQuery('body').trigger('layoutresizeall');
    });

    // wait a few ms for it to render, then trigger initial layout
    setTimeout(function() {
        jQuery('body').trigger('layoutresizeall');
    },100);

    // if a filename was specified, load it & clean up the temporary variable
    if (openmdao.edit_filename) {
        code.editFile(openmdao.edit_filename);
        delete openmdao.edit_filename;
    }

    // save ref to editor for others to use
    openmdao.model.editor = code;
});

