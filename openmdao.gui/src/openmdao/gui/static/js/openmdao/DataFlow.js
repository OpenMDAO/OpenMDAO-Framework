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
        dataflow  = new draw2d.Workflow(id),
        figures = {}
        
        
    // set background image
    dataflow.setBackgroundImage( "/static/images/grid_10.png", true)
    
    // make the dataflow pane droppable
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

    /** update dataflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        var comps = json[0],
            conns = json[1],
            x = 50, y = 50
        
        dataflow.clear();
        
        jQuery.each(comps,function(idx,name) {
            // FIXME: just getting a name atm, also want type I think
            var typepath = name 
            if (typepath !== undefined) {
                var tokens = typepath.split('.'),
                    typename = tokens[tokens.length-1],
                    fig = new openmdao.ComponentFigure(name,typename)
                    
                fig.setTitle(name)
                dataflow.addFigure(fig,x,y)
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
                c = new draw2d.Connection()
            c.setSource(src_fig.getPort("output"));
            c.setTarget(dst_fig.getPort("input"));
            dataflow.addFigure(c);
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
