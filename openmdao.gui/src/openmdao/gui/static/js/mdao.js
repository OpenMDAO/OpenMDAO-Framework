/**
 * stuff to do after the page is loaded
 */
 
// create interface to openmdao
// TODO:  get this out of globals.. currently just here for menu access
var model = new openmdao.Model();
    
jQuery(document).ready(function() {
    // add menu
    new openmdao.Menu("menu");
    
    // add tabbed pane functionality
    new openmdao.TabbedPane("leftcol_tabs");
    new openmdao.TabbedPane("central_tabs");
    new openmdao.TabbedPane("rightcol_tabs");

    // add gui functionality to designated DOM nodes
    new openmdao.ObjectTree("otree",model,
        new openmdao.PropertiesEditor("propertieseditor",model).editObject)
    new openmdao.FileTree("ftree",model,
        new openmdao.CodeEditor("texteditor",model).editFile)
    new openmdao.Palette("palette",model)
    new openmdao.DataFlow("dataflow",model)
    new openmdao.Console("cmdform","command","history",model);

    // experimental
    new openmdao.Plotter("plotter",model)
    new openmdao.ThreeDStuff("threed",model)
    
    // initialize views
    model.updateListeners();
});
