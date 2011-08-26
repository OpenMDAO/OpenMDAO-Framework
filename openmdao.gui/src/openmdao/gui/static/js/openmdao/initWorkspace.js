/**
 * stuff to do after the page is loaded
 */
 
// create interface to openmdao
// TODO:  get this out of globals.. currently just here for menu access
var model = new openmdao.Model();
var layout
    
jQuery(function() {
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
    (function() {
        var select_fn = new openmdao.PropertiesEditor("propertieseditor",model).editObject,
            dblclk_fn = function(path) {
                new openmdao.ComponentEditor(model,path).editObject
            }        
        new openmdao.ObjectTree("otree",model,select_fn,dblclk_fn)
        
        var edit_fn = new openmdao.CodeEditor("code",model).editFile,
            view_fn = function(path) {
                openmdao.Util.popupWindow('geometry?path='+path,'Geometry',600,800)
            }
        new openmdao.FileTree("ftree",model,edit_fn,view_fn)
        
        new openmdao.Palette("palette",model)
        new openmdao.WorkflowDiagram("workflow",model)
        new openmdao.Console("cmdform","command","history",model);
    })()

    // initialize views
    model.updateListeners();
    
    // start with objects, workflow & properties visible
    jQuery('#otree_tab').click();
    jQuery('#workflow_tab').click();
    jQuery('#properties_tab').click();
});
