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
        
    // add menu
    new openmdao.Menu("menu");
    
    // add tabbed pane functionality
    new openmdao.TabbedPane("leftcol_tabs");
    new openmdao.TabbedPane("central_tabs");
    new openmdao.TabbedPane("rightcol_tabs");

    // add gui functionality to designated DOM nodes
    new openmdao.ObjectTree("otree",model,
        new openmdao.PropertiesEditor("propertieseditor",model).editObject,
        openmdao.PopupPropertiesEditor)
    new openmdao.FileTree("ftree",model,
        new openmdao.CodeEditor("texteditor",model).editFile)
    new openmdao.Palette("palette",model)
    new openmdao.WorkflowDiagram("workflow",model)
    new openmdao.Console("cmdform","command","history",model);

    // experimental
    new openmdao.Plotter("plotter",model)
    new openmdao.O3DViewer("threed",model)
    
    // initialize views
    model.updateListeners();
});
