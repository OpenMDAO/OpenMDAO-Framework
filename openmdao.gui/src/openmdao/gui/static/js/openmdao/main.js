/**
 * stuff to do after the page is loaded
 */


jQuery(function() {
    // define openmdao namespace & create interface to openmdao in global scope
    openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;
    openmdao.model = new openmdao.Model();

    // set the layout (note: global scope)
    layout = jQuery('body').layout({
        north__size: 40,
        north__resizable: false,
        north_showOverflowOnHover: true,
        south__size: 150
    });

    // add main menu
    jQuery.getJSON("/static/js/openmdao/MainMenu.json",
        function(json) { new openmdao.Menu("menu",json); }
    );

    // add tabbed pane functionality
    openmdao.TabbedPane("leftcol_tabs");
    openmdao.TabbedPane("central_tabs");
    openmdao.TabbedPane("rightcol_tabs");

    // add gui functionality to designated DOM nodes
    (function() {
        var model = openmdao.model;

        var data = new openmdao.DataflowFrame("dataflow",model,''),
            work = new openmdao.WorkflowFrame("workflow",model,''),
            prop = new openmdao.PropertiesFrame("propertieseditor",model);

        // create functions to load content into the different panes
        // intercept tab clicks to set the adjacent label
        var central_label = jQuery('#central_label'),
            dataflow_tab  = jQuery('#dataflow_tab'),
            workflow_tab  = jQuery('#workflow_tab');

        dataflow_tab.click(function(e) { central_label.text(data.getPathname()); });
        workflow_tab.click(function(e) { central_label.text(work.getPathname()); });
                
        function data_fn(path) { data.showDataflow(path); dataflow_tab.click(); }
        function work_fn(path) { work.showWorkflow(path); workflow_tab.click(); }
        function prop_fn(path) { prop.editObject(path); }
        function comp_fn(path) { new openmdao.ComponentFrame(model,path); }

        new openmdao.ComponentTreeFrame("otree", model, prop_fn, comp_fn, work_fn, data_fn);
        new openmdao.PaletteFrame("palette",  model);
        new openmdao.ConsoleFrame("console",  model);
    }());

    // start with objects, dataflow & properties visible
    jQuery('#otree_tab').click();
    jQuery('#dataflow_tab').click();
    jQuery('#properties_tab').click();
});
