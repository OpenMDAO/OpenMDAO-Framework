
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.WorkflowPane = function(elm,model,pathname,name,editable) {
    // initialize private variables
    var self = this,
        comp_figs = {},
        flow_figs = {},
        workflowID = "#"+pathname.replace(/\./g,'-')+"-workflow",
        workflowDiv = jQuery('<div id='+workflowID+' style="height:'+(screen.height-100)+'px;width:'+(screen.width-100)+'px">').appendTo(elm),
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
        accept: '.obj',
        drop: function(ev,ui) { 
            debug.info("Workflow drop:",ev,ui)
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("path"),
                off = workflowDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top),
                flowfig = workflow.getBestCompartmentFigure(x,y);
            debug.info("Workflow dropped object:",droppedObject)            
            if (flowfig && droppedObject.hasClass('obj')) {
                debug.info('flowfig:',flowfig,'pathname:',flowfig.pathname)
                model.issueCommand('top'+flowfig.pathname+'.workflow.add("'+droppedPath+'")')
            }
            else {
                debug.info('Workflow drop was not on a flow figure')
            }
        }
    });
    
    /** expand workflow (container) figures to contain all their children */
    function resizeFlowFigures() {
        var figWidth = 110,figHeight = 60
        for (fig in flow_figs) {
            var i=0, xmin=999999, xmax=0, ymin=999999, ymax=0,
                children = flow_figs[fig].getChildren()
            for (i=0;i<children.size;i++) {
                child = children.get(i);                
                x = child.getAbsoluteX();
                if (x < xmin) {
                    xmin = x;
                }
                if (x > xmax) {
                    xmax = x;
                }
                y = child.getAbsoluteY();
                if (y < ymin) {
                    ymin = y;
                }
                if (y > ymax) {
                    ymax = y;
                }                    
            }
            flow_figs[fig].setDimension(xmax+figWidth-xmin,ymax+figHeight-ymin)
        }
    }
    
    /** get the width of the flow container */
    function getFlowWidth(flow_fig) {
        var figWidth = 100,
            i=0, xmin=999999, xmax=0,
            children = flow_fig.getChildren()
        if (children.size > 0) {
            for (i=0;i<children.size;i++) {
                child = children.get(i);                
                x = child.getAbsoluteX();
                if (x < xmin) {
                    xmin = x;
                }
                if (x > xmax) {
                    xmax = x;
                }
            }
            return xmax+figWidth-xmin;
        }
        else
            return 0;
    }
    
    /** get the height of the flow container */
    function getFlowHeight(flow_fig) {
        var figHeight = 50,
            i=0, ymin=999999, ymax=0,
            children = flow_fig.getChildren()
        if (children.size > 0) {
            for (i=0;i<children.size;i++) {
                child = children.get(i);                
                y = child.getAbsoluteY();
                if (y < ymin) {
                    ymin = y;
                }
                if (y > ymax) {
                    ymax = y;
                }
            }
            return ymax+figHeight-ymin;
        }
        else
            return 0;
    }
    
    /** get the coordinates of the next location to place a figure */
    function getNextCoords() {
        // stagger left to right, top to bottom
        var count = Object.keys(comp_figs).length,
            width = 100,
            height = 50,
            space = 20
            x = count*(width+space),
            y = count*(height+space)
        return { 'x': x, 'y': y }
    }

    /** get the coordinates to place a figure */
    function getCoords(flow,comp,horizontal) {
        // stagger left to right, top to bottom
        var count = Object.keys(comp_figs).length,
            width = 100,
            height = 50,
            space = 20
            x = count*(width+space),
            y = count*(height+space)
        return { 'x': x, 'y': y }
    }
    
    /** update workflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(flow_name,json,horizontal) {
        var path = json['pathname'],
            type = json['type'],
            drvr = json['driver'],
            flow = json['workflow'],
            asm  = openmdao.Util.getParentPath(path),
            comp_fig, flow_fig, flowpath, count
        
        if (flow) {
            // add driver figure
            comp_fig = new openmdao.WorkflowComponentFigure(model,path,type);
            comp_figs[path] = comp_fig;
            
            flow_fig = flow_figs[flow_name];
            if (flow_fig) {
                //debug.info("flow:",flow_name,"children:",flow_fig.getChildren())
                count = flow_fig.getChildren().size;
                if (horizontal) {
                    //x = flow_fig.getAbsoluteX()+getFlowWidth(flow_fig);
                    x = flow_fig.getAbsoluteX()+comp_fig.getWidth()*count*2;
                    y = 20+flow_fig.getAbsoluteY();
                }
                else {
                    x = flow_fig.getAbsoluteX();
                    //y = 20+flow_fig.getAbsoluteY()+getFlowHeight(flow_fig);
                    y = 20+flow_fig.getAbsoluteY()+comp_fig.getHeight()*count*2;
                }                                            
                flow_fig.addChild(comp_fig)
            }
            else {
                x = 50;
                y = 50;
            }
            //debug.info("FLOW => flow:",flow_name,"comp:",path,"count:",count,"horiz:",horizontal,"x:",x,"y:",y)
            workflow.addFigure(comp_fig,x,y)

            // add workflow compartment figure for this flow (overlap bottom right of driver figure)
            flowpath = flow_name+'.'+path
            flow_fig = new openmdao.WorkflowFigure(model,flowpath,path);
            workflow.addFigure(flow_fig,x+comp_fig.getWidth()-20,y+comp_fig.getHeight()-10);
            if (flow_figs[flow_name]) {
                flow_figs[flow_name].addChild(flow_fig);
            }            
            flow_figs[flowpath] = flow_fig;

            jQuery.each(flow,function(idx,comp) {
                updateFigures(flowpath,comp,!horizontal)
            })
        }
        else if (drvr) {
            // don't add a figure for an assembly, it will be represented by it's driver
            updateFigures(flow_name,drvr,!horizontal)
        }
        else {
            // add component figure
            comp_fig = new openmdao.WorkflowComponentFigure(model,path,type)
            comp_figs[path] = comp_fig

            flow_fig = flow_figs[flow_name];
            if (flow_fig) {
                count = flow_fig.getChildren().size;                    
                if (horizontal) {
                    //x = flow_fig.getAbsoluteX()+getFlowWidth(flow_fig);
                    x = flow_fig.getAbsoluteX()+comp_fig.getWidth()*count*2;
                    y = 20+flow_fig.getAbsoluteY();
                }
                else {
                    x = flow_fig.getAbsoluteX();
                    //y = 20+flow_fig.getAbsoluteY()+getFlowHeight(flow_fig);
                    y = 20+flow_fig.getAbsoluteY()+comp_fig.getHeight()*count*2;
                }                                            
                flow_fig.addChild(comp_fig)
            }
            else {
                x = 50;
                y = 50;
            }
            
            //debug.info("COMP => flow:",flow_name,"comp:",path,"count:",count,"horiz:",horizontal,"x:",x,"y:",y)

            workflow.addFigure(comp_fig,x,y)
        }
    }

    /** update workflow diagram */
    this.loadData = function(json) {
        workflow.clear()
        comp_figs = {}
        flow_figs = {}
        if (Object.keys(json).length > 0) {
            updateFigures('',json,false)
            resizeFlowFigures()
        }
    }
    
}
