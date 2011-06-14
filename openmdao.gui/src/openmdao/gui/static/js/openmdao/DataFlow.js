/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.DataFlow = function(id,model) {
    /***********************************************************************
     *  private
     ***********************************************************************/
    // FIXME: really workflow, but there was a naming conflict with draw2d
    var self = this,
        elm = jQuery("#"+id).width(screen.width).height(screen.height),
        workflow  = new draw2d.Workflow(id),
        figures = {}
        
    // set background image
    workflow.setBackgroundImage( "/static/images/grid_10.png", true)
    
    // context menu
    workflow.getContextMenu=function(){
        var menu=new draw2d.Menu();
        menu.appendMenuItem(new draw2d.MenuItem("Show Grid",null,function(x,y){
            workflow.setGridWidth(10,10);
            workflow.setBackgroundImage("/static/images/grid_10.png",true);
        }));
        menu.appendMenuItem(new draw2d.MenuItem("Hide Grid",null,function(x,y){
            workflow.setBackgroundImage(null,false);
        }));
        menu.appendMenuItem(new draw2d.MenuItem("Add Note",null,function(x,y){
            var annotation = new draw2d.Annotation("NOTE: ");
            annotation.setDimension(250,70);
            var off = elm.parent().offset()
            x = Math.round(x - off.left)
            y = Math.round(y - off.top)
            workflow.addFigure(annotation,x,y);
        }));
        
        return menu;
    };
    
    // toolbar may be useful at some point?
    // var tbar = new openmdao.Toolbar();
    // workflow.showDialog(tbar,400,10);
    
    // make the workflow pane droppable
    elm.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone()
            var off = elm.parent().offset()
            x = Math.round(ui.offset.left - off.left)
            y = Math.round(ui.offset.top - off.top)
            // get the type name and path
            var typename = droppedObject.text()
            var typepath = droppedObject.attr("path")
            openmdao.Util.promptForName(function(name) { 
                model.addComponent(typepath,name,x,y)
            })
        }
    });

    /** update workflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        var comps = json[0],
            conns = json[1],
            x = 50, y = 50
        
        workflow.clear();
        
        jQuery.each(comps,function(idx,name) {
            // FIXME: just getting a name atm, also want type I think
            var typepath = name 
            if (typepath !== undefined) {
                var tokens = typepath.split('.'),
                    typename = tokens[tokens.length-1],
                    fig = new openmdao.ComponentFigure(model,name,typename)
                    
                fig.setTitle(name)
                workflow.addFigure(fig,x,y)
                figures[name] = fig
                
                x = x+125;
                y = y+100;                    
            }
        })
        
        jQuery.each(conns,function(idx,conn) {
            var src_name = conn[0].split('.')[0],
                dst_name = conn[1].split('.')[0],
                src_fig = figures[src_name],
                dst_fig = figures[dst_name],
                c = new openmdao.ContextMenuConnection()
            c.setSource(src_fig.getPort("output"));
            c.setTarget(dst_fig.getPort("input"));
            workflow.addFigure(c);
        })
    }
    
    /** update the schematic, with data from the model */
    function update() {
        model.getWorkflow(updateFigures)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)

    /***********************************************************************
     *  privileged
     ***********************************************************************/
        
}
