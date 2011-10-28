/**
 * stuff to do after the page is loaded
 */
 
    
jQuery(function() {
    // define openmdao namespace & create interface to openmdao in global scope
    openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 
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
        function(json) { new openmdao.Menu("menu",json) }
    )
    
    // add tabbed pane functionality
    openmdao.TabbedPane("leftcol_tabs");
    openmdao.TabbedPane("central_tabs");
    openmdao.TabbedPane("rightcol_tabs");

    // add gui functionality to designated DOM nodes
    (function() {
        var model = openmdao.model;
        
        var data_fn = new openmdao.DataflowDiagram("dataflow",model,'').showDataflow;        
        var work_fn = new openmdao.WorkflowDiagram("workflow",model,'driver').showWorkflow;
        var code_fn = new openmdao.CodeEditor("code",model).editFile;
        var prop_fn = new openmdao.PropertiesEditor("propertieseditor",model).editObject;
        
        var geom_fn = function(path) { openmdao.Util.popupWindow('geometry?path='+path,'Geometry',600,800) }
        var comp_fn = function(model,path) { new openmdao.ComponentEditor(model,path) };
        
        new openmdao.ObjectTree("otree", model, prop_fn, comp_fn, work_fn, data_fn);
        new openmdao.FileTree("ftree",   model, code_fn, geom_fn);        
        new openmdao.Palette("palette",  model)        
        new openmdao.Console("cmdform", "command", "history", model);
        
        // initialize views
        model.updateListeners();
    })()

    
    // start with objects, workflow & properties visible
    jQuery('#otree_tab').click();
    jQuery('#dataflow_tab').click();
    jQuery('#properties_tab').click();
});
