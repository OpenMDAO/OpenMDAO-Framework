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
        figures = {}
        updateFigures('',json)
    }
    
    /** update workflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(asm,json) {
        var path = json['pathname'],
            type = json['type'],
            drvr = json['driver'],
            flow = json['workflow'],
            fig 

        debug.info('=====>',asm,json,path,type,drvr,flow)
            
        if (drvr) {
            // add driver figure
            fig = new openmdao.ComponentFigure(model,drvr['pathname'],drvr['type'])
            var count = Object.keys(comp_figs).length,
                x = count*(fig.getWidth()+20)  + 20,
                y = count*(fig.getHeight()+20) + 20
            debug.info('adding driver',asm,fig,'to workflow',x,y)
            workflow.addFigure(fig,x,y)
            comp_figs[drvr['pathname']] = fig
            if (flow_figs[asm]) {
                flow_figs[asm].addChild(fig)
                var c = flow_figs[asm].getChildren().length
                flow_figs[asm].setDimension(c*(fig.getWidth()+20)+20,
                                            c*(fig.getHeight()+20)+20)
            }
                
            // add workflow (compartment) figure
            var count = Object.keys(comp_figs).length,
                x = count*(fig.getWidth()+20)  + 20,
                y = count*(fig.getHeight()+20) + 20
            fig = new openmdao.WorkflowFigure(model,path,type)
            debug.info('adding flow',asm,fig,'to workflow',x,y)
            workflow.addFigure(fig,x,y)
            flow_figs[path] = fig
            jQuery.each(flow,function(idx,comp) {
                updateFigures(path,comp)
            })
        }
        else {
            // add component figure
            fig = new openmdao.ComponentFigure(model,path,type)
            var count = Object.keys(comp_figs).length,
                x = count*(fig.getWidth()+20)  + 20,
                y = count*(fig.getHeight()+20) + 20
            debug.info('adding comp',asm,fig,'to workflow',x,y)
            workflow.addFigure(fig,x,y)
            comp_figs[path] = fig
            if (flow_figs[asm]) {
                flow_figs[asm].addChild(fig)
                var c = flow_figs[asm].getChildren().size
                debug.info('resize',asm,flow_figs[asm],'for',c)
                flow_figs[asm].setDimension(c*(fig.getWidth()+20)+20,
                                            c*(fig.getHeight()+20)+20)
            }
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
