/**
 * stuff to do after the editor editor page is loaded
 */

jQuery(function() {
    if (opener && opener.openmdao) {
        openmdao.project = opener.openmdao.project;
    }
    else {
        // define openmdao namespace & create interface to openmdao in global scope
        openmdao = (typeof openmdao === 'undefined' || !openmdao ) ? {} : openmdao ;
        openmdao.project = new openmdao.Project();
    }

    var editor = new openmdao.CodeFrame('code_pane', openmdao.project);
    var ftree = new openmdao.FileTreeFrame('file_pane', openmdao.project);

    // allow frames to close in an orderly fashion before closing window
    jQuery(window).bind('beforeunload', function(e) {
        openmdao.project.codeEditor = undefined;
        editor.close();
        ftree.close();
    });

    // set the layout
    jQuery('body').layout({
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
            editor.resize();
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
        editor.editFile(openmdao.edit_filename);
        delete openmdao.edit_filename;
    }

    // save ref to editor for others to use
    openmdao.project.codeEditor = editor;
});

