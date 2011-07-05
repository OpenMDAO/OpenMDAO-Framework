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
        comp_figs = {},
        flow_figs = {}
        
    // set background image
    workflow.setBackgroundImage( "/static/images/grid_10.png", true)
    
    /** / context menu
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
    /**/
    
    /** / toolbar may be useful at some point?
    var tbar = new openmdao.Toolbar();
    workflow.showDialog(tbar,400,10);
    /**/
    
    // make the workflow pane droppable
    elm.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                typename = droppedObject.text(),
                typepath = droppedObject.attr("path"),
                off = elm.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top)
            openmdao.Util.promptForName(function(name) { 
                model.addComponent(typepath,name,x,y)
            })
        }
    });

    /** update workflow for top level assembly */
    function updateWorkflow(json) {
        workflow.clear();
        comp_figs = {},
        flow_figs = {},
        updateFigures(json)
        var count,
            figWidth = 100,
            figHeight = 50
        for (fig in flow_figs) {
            debug.info('resize',flow_figs[fig])
            count = flow_figs[fig].getChildren().size
            debug.info('resize',flow_figs[fig],'for',count)
            flow_figs[fig].setDimension(count*(figWidth+20)+40,
                                        count*(figHeight+20)+40)
        }
    }
    
    /** get the coordinates of the next location to place a figure */
    function getNextCoords() {
        var count = Object.keys(comp_figs).length,
            figWidth = 100,
            figHeight = 50
            x = count*(figWidth+20)  + 20,
            y = count*(figHeight+20) + 20
        return { 'x': x, 'y': y }
    }
    
    /** update workflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        var path = json['pathname'],
            type = json['type'],
            drvr = json['driver'],
            flow = json['workflow'],
            tok, asm, fig, coords
        
        // get the parent assembly name 
        var lastdot = path.lastIndexOf('.')
        if (lastdot > 0)
            asm = path.substring(0,lastdot)
        else
            asm = ''

        debug.info('=====>',asm,json,path,type,drvr,flow)
            
        if (flow) {
            // add driver figure
            fig = new openmdao.ComponentFigure(model,path,type)
            coords = getNextCoords()
            debug.info('adding driver',asm,fig,'to workflow',coords['x'],coords['y'])
            comp_figs[path] = fig
            if (flow_figs[asm]) {
                workflow.addFigure(fig,coords['x']+20,coords['y']+20)
                flow_figs[asm].addChild(fig)
                var c = flow_figs[asm].getChildren().length
                flow_figs[asm].setDimension(c*(fig.getWidth()+20)+40,
                                            c*(fig.getHeight()+20)+40)
            }
            else
                workflow.addFigure(fig,coords['x'],coords['y'])

            jQuery.each(flow,function(idx,comp) {
                updateFigures(comp)
            })
        }
        else if (drvr) {
            // add assembly (compartment) figure
            fig = new openmdao.WorkflowFigure(model,path,type)
            coords = getNextCoords()
            debug.info('adding assembly/flow',asm,fig,'to workflow',coords['x'],coords['y'])
            if (flow_figs[asm]) {
                flow_figs[asm].addChild(fig)
                var c = flow_figs[asm].getChildren().length
                flow_figs[asm].setDimension(c*(fig.getWidth()+20)+40,
                                            c*(fig.getHeight()+20)+40)                
            }
            workflow.addFigure(fig,coords['x'],coords['y'])
            flow_figs[path] = fig
            updateFigures(drvr)
        }
        else {
            // add component figure
            fig = new openmdao.ComponentFigure(model,path,type)
            coords = getNextCoords()
            debug.info('adding comp',asm,fig,'to workflow',coords['x'],coords['y'])
            comp_figs[path] = fig
            if (flow_figs[asm]) {
                workflow.addFigure(fig,coords['x']+20,coords['y']+20)
                flow_figs[asm].addChild(fig)
                var c = flow_figs[asm].getChildren().size
                debug.info('resize',asm,flow_figs[asm],'for',c)
                flow_figs[asm].setDimension(c*(fig.getWidth()+20)+40,
                                            c*(fig.getHeight()+20)+40)
            }
            else
                workflow.addFigure(fig,coords['x'],coords['y'])
        }
        
        
        /**
        jQuery.each(json['connections'],function(idx,conn) {
            var src_name = conn[0].split('.')[0],
                dst_name = conn[1].split('.')[0],
                c = new openmdao.ContextMenuConnection()
            if (conn[0].indexOf('.') < 0)
                src_name = asm_name
            var src_fig = figures[src_name]
            if (conn[1].indexOf('.') < 0)
                dst_name = asm_name
            var dst_fig = figures[dst_name]
            c.setSource(src_fig.getPort("output"));
            c.setTarget(dst_fig.getPort("input"));
            workflow.addFigure(c);
        })
        /**/        
    }
    
    /** update the schematic with data from the model */
    function update() {
        model.getWorkflow(updateWorkflow)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)

    /***********************************************************************
     *  privileged
     ***********************************************************************/
        
}
