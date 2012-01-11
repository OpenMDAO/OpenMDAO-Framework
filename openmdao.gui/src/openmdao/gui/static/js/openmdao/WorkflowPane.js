
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.WorkflowPane = function(elm,model,pathname,name,editable) {
    // initialize private variables
    var self = this,
        comp_figs = {},
        flow_figs = {},
        workflowID = "#"+pathname.replace(/\./g,'-')+"-workflow",
        workflowCSS = 'height:'+(screen.height-100)+'px;width:'+(screen.width-100)+'px;overflow:auto;',
        workflowDiv = jQuery('<div id='+workflowID+' style="'+workflowCSS+'">').appendTo(elm),
        workflow = new draw2d.Workflow(workflowID);
        
    self.pathname = pathname;
        
    workflow.setBackgroundImage( "/static/images/grid_10.png", true);
        
    /** FIXME: workflow context menu conflicts with figure context menu ** /
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
        // menu.appendMenuItem(new draw2d.MenuItem("Add Note",null,function(x,y){
            // var annotation = new draw2d.Annotation("NOTE: ");
            // annotation.setDimension(250,70);
            // var off = workflowDiv.parent().offset()
            // x = Math.round(x - off.left)
            // y = Math.round(y - off.top)
            // workflow.addFigure(annotation,x,y);
        // }));
        
        return menu;
    };
    /**/
    
    /** / toolbar may be useful at some point?
    var tbar = new openmdao.Toolbar();
    workflow.showDialog(tbar,400,10);
    /**/
    
    // make the workflow pane droppable
    workflowDiv.droppable ({
        accept: '.obj, .objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("path"),
                off = workflowDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top),
                flowfig = workflow.getBestCompartmentFigure(x,y),
                bestfig = workflow.getBestFigure(x,y);
            debug.info(droppedName,'dropped on',self.pathname,'workflow');
            if (flowfig && droppedObject.hasClass('obj')) {
                model.issueCommand('top'+flowfig.pathname+'.workflow.add("'+droppedPath+'")')
            }
            else if (droppedObject.hasClass('objtype') && (/^openmdao.lib.drivers./).test(droppedPath)) {
                // TODO: really need interface info to check if the type and fig are drivers
                if (bestfig instanceof openmdao.WorkflowComponentFigure && openmdao.Util.getName(bestfig.pathname) === 'driver') {
                    path = openmdao.Util.getPath(bestfig.pathname);
                    // TODO: need a 'replaceDriver' function to preserve driver config
                    model.addComponent(droppedPath,'driver',path);
                }
                else {
                    debug.info(droppedPath,'was not dropped on a driver', bestfig);
                }
            }
            else {
                debug.info('Workflow drop was not valid (obj on flow or objtype on driver)')
            }
        }
    });
    
    /** expand workflow (container) figures to contain all their children */
    function resizeFlowFigures() {
        for (fig in flow_figs) {
            flow_figs[fig].resize();
        }
    }

    /** update workflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(flow_name,json) {
        var path = json['pathname'],
            type = json['type'],
            drvr = json['driver'],
            flow = json['workflow'],
            asm  = openmdao.Util.getPath(path),
            comp_fig, flow_fig, flowpath, newflow_fig, count, x, y;
        
        if (flow) {
            // add driver figure
            comp_fig = new openmdao.WorkflowComponentFigure(model,path,type);
            comp_figs[path] = comp_fig;
            
            flow_fig = flow_figs[flow_name];
            if (flow_fig) {
                flow_fig.addComponentFigure(comp_fig);
            }
            else {
                workflow.addFigure(comp_fig,50,50);
            }

            // add workflow compartment figure for this flow (overlap bottom right of driver figure)
            flowpath = flow_name+'.'+path
            newflow_fig = new openmdao.WorkflowFigure(model,flowpath,path,comp_fig);
            x = comp_fig.getAbsoluteX()+comp_fig.getWidth()-20;
            y = comp_fig.getAbsoluteY()+comp_fig.getHeight()-10;
            workflow.addFigure(newflow_fig,x,y);
            if (flow_fig) {
                newflow_fig.horizontal = !flow_fig.horizontal;
                flow_fig.addChild(newflow_fig);
            }
            flow_figs[flowpath] = newflow_fig;

            jQuery.each(flow,function(idx,comp) {
                updateFigures(flowpath,comp);
            })
        }
        else if (drvr) {
            // don't add a figure for an assembly, it will be represented by it's driver
            updateFigures(flow_name,drvr);
        }
        else {
            // add component figure
            comp_fig = new openmdao.WorkflowComponentFigure(model,path,type);
            comp_figs[path] = comp_fig;

            flow_fig = flow_figs[flow_name];
            if (flow_fig) {
                flow_fig.addComponentFigure(comp_fig);
            }
            else {
                workflow.addFigure(comp_fig,50,50);
            }
        }
    }

    /** update workflow diagram */
    this.loadData = function(json) {
        workflow.clear()
        comp_figs = {}
        flow_figs = {}
        if (Object.keys(json).length > 0) {
            updateFigures('',json);
            resizeFlowFigures();
        }
    }
    
}
