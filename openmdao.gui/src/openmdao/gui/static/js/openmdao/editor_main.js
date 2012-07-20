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

    // set the layout (note: global scope)
    layout = jQuery('body').layout({});

    var code = new openmdao.CodeFrame('code_pane', openmdao.model);

    function code_fn(path) { code.editFile(path); }
    function geom_fn(path) { openmdao.Util.popupWindow('geometry?path='+path,'Geometry'); }

    var ftree = new openmdao.FileTreeFrame('file_pane', openmdao.model, code_fn, geom_fn);

    // allow frames to close in an orderly fashion before closing window
    jQuery(window).bind('beforeunload', function(e) {
        code.close();
        ftree.close();
    });


    // resize the Ace code pane when the window is resized
    jQuery(window).resize(function(e) {
        code.resize();
    });

    // set label above code editor to filename when tab is clicked
    //code_tab.click(function(e) { central_label.text(code.getPathname()); });

    // make sure tabbed panes are showing
    //code_tab.click();
    //file_tab.click();

    // do layout
    jQuery('body').trigger('layoutresizeall');
});

