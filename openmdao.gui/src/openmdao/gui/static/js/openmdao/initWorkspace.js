/**
 * stuff to do after the page is loaded
 */
 
// create interface to openmdao
// TODO:  get this out of globals.. currently just here for menu access
var model = new openmdao.Model();
var layout
    
jQuery(document).ready(function() {
    // set the layout
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
    new openmdao.TabbedPane("leftcol_tabs");
    new openmdao.TabbedPane("central_tabs");
    new openmdao.TabbedPane("rightcol_tabs");

    // add gui functionality to designated DOM nodes
    new openmdao.ObjectTree("otree",model,
        new openmdao.PropertiesEditor("propertieseditor",model).editObject,
        openmdao.PopupPropertiesEditor
    )
    new openmdao.FileTree("ftree",model,
        new openmdao.CodeEditor("code",model).editFile,
        function(path) { new openmdao.O3DViewer("geometry",model,path) }  // create when needed
    )
    new openmdao.Palette("palette",model)
    new openmdao.WorkflowDiagram("workflow",model)
    new openmdao.Console("cmdform","command","history",model);

    // initialize views
    model.updateListeners();
    
    // start with objects, workflow & properties visible
    jQuery('#otree_tab').click();
    jQuery('#workflow_tab').click();
    jQuery('#properties_tab').click();
});
