/**
 * stuff to do after the page is loaded
 */

jQuery(function() {
    // define openmdao namespace & create interface to openmdao in global scope
    openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;
    var listeners_ready = jQuery.Deferred();
    openmdao.project = new openmdao.Project(listeners_ready);
    openmdao.drag_and_drop_manager = new openmdao.DragAndDropManager();

    // register value editors for supported OpenMDAO data types
    openmdao.ValueEditor.registerEditor("str", Slick.Editors.Text);
    openmdao.ValueEditor.registerEditor("bool", BoolEditor);
    openmdao.ValueEditor.registerEditor("float", Slick.Editors.Text);
    openmdao.ValueEditor.registerEditor("int", Slick.Editors.Integer);
    openmdao.ValueEditor.registerEditor("enum", EnumEditor);
    openmdao.ValueEditor.registerEditor("dict", DictEditor);
    openmdao.ValueEditor.registerEditor("ndarray", ArrayEditor);

    // binary messages have the first NAME_SIZE bytes allocated for a topic name
    // (e.g. for the geometry viewer)
    openmdao.NAME_SIZE = 256;

    // set the layout (note: global scope, referenced in workspace html)
    layout = jQuery('body').layout({
        north__size: 40,
        north__resizable: false,
        north_showOverflowOnHover: true,
        south__size: 150,
        onresize: function(e) {
            // resize content pane of all tabbed panes to fill the layout pane
            var layout_pane = jQuery('.ui-layout-'+e),
                tabs_height = layout_pane.find('.ui-tabs-nav').outerHeight(),
                pane_height = layout_pane.height()-tabs_height,
                pane_width  = layout_pane.width();
            jQuery(layout_pane.find('.ui-tabs-panel').each(function() {
                var panel = jQuery(this);
                panel.height(pane_height);
                panel.width(pane_width);
            }));
        }
    });

    // add main menu
    jQuery.getJSON("/static/js/openmdao/MainMenu.json",
        function(json) { new openmdao.Menu("menu",json); }
    );

    // add tabbed pane functionality
    jQuery("#leftcol_tabs").tabs();
    jQuery("#central_tabs").tabs();
    jQuery("#rightcol_tabs").tabs();

    // add gui functionality to designated DOM nodes
    (function() {
        var project = openmdao.project;
        new openmdao.ConsoleFrame("console",  project);

        var prop = new openmdao.PropertiesFrame("properties_pane", project);
        function prop_fn(path) { prop.editObject(path); }

        var data = new openmdao.DataflowFrame("dataflow_pane", project,'', prop_fn),
            work = new openmdao.WorkflowFrame("workflow_pane", project,'');

        // create functions to load content into the different panes
        // intercept tab clicks to set the adjacent label
        var central_label = jQuery('#central_label'),
            dataflow_tab  = jQuery('#dataflow_tab a'),
            workflow_tab  = jQuery('#workflow_tab a'),
            property_tab  = jQuery('#properties_tab a');

        dataflow_tab.click(function(e) {
            central_label.text(data.getPathname());
        });
        workflow_tab.click(function(e) {
            central_label.text(work.getPathname());
            // unfortunately, can't compute background until element is visible
            jQuery('.WorkflowFigure').trigger('setBackground');
        });
        property_tab.click(function(e) {
            // slickgrid fails to render some cells when tab is hidden
            // this will trigger rendering when tab is clicked
            prop.resize_contents();
        });

        function data_fn(path) { data.showDataflow(path); dataflow_tab.click(); }
        function work_fn(path) { work.showWorkflow(path); workflow_tab.click(); }
        function comp_fn(path) { new openmdao.ObjectFrame(project,path); }

        new openmdao.ObjectTreeFrame("otree_pane", project, prop_fn, comp_fn, work_fn, data_fn);
        new openmdao.FileTreeFrame("ftree_pane", project);
        new openmdao.LibraryFrame("library_pane",  project);

        listeners_ready.resolve();
    }());

    // do layout
    jQuery('body').trigger('layoutresizeall');
});


