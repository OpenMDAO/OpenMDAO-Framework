/**
 * stuff to do after the code editor is loaded
 */
 
    
jQuery(function() {
    // define openmdao namespace & create interface to openmdao in global scope
    openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 
    openmdao.model = new openmdao.Model();

    // set the layout (note: global scope)
    layout = jQuery('body').layout({
    });

    // add tabbed pane functionality
    openmdao.TabbedPane("leftcol_tabs");
    openmdao.TabbedPane("central_tabs");
    //openmdao.TabbedPane("rightcol_tabs");

    // add gui functionality to designated DOM nodes
    (function() {
        if (opener && opener.openmdao && opener.openmdao.model ) {
            openmdao.model = opener.openmdao.model;
        }
        else {
            openmdao.model = new openmdao.Model();
        }
        
        var model = openmdao.model;
        
        var code = new openmdao.CodeEditor("code",model);

        // create functions to load content into the different panes
        // intercept tab clicks to set the adjacent label
        var central_label = jQuery('#central_label'),
            code_tab      = jQuery('#code_tab');

        code_tab.click(function(e) { central_label.text(code.getPathname()); })

        function code_fn(path) { code.editFile(path); code_tab.click(); }

        new openmdao.FileTree("ftree", model, code_fn);
    })()

    jQuery('#code_tab').click();
    jQuery('#ftree_tab').click();
    jQuery("body").trigger("layoutresizeall");

});
