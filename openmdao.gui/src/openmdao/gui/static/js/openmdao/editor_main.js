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

    // do layout
    jQuery('body').trigger('layoutresizeall');
});

