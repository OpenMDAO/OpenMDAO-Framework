
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.WorkflowDiagram = function(id,model) {
    openmdao.WorkflowDiagram.prototype.init.call(this,id,'Workflows',[]);
    
    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        comp_figs = {},
        flow_figs = {},
        workflowID = "#"+id+"-workflow",
        workflowDiv = jQuery('<div id='+workflowID+' style="height:'+(screen.height-100)+'px;width:'+(screen.width-100)+'px">').appendTo('#'+id),
        workflow = new draw2d.Workflow(workflowID)
        
    workflow.setBackgroundImage( "/static/images/grid_10.png", true)
        
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
            debug.info("Workflow drop:",ev,ui)
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("path"),
                off = workflowDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top)
            debug.info("dropped:",droppedObject)
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForName(function(name) { 
                    model.addComponent(droppedPath,name,x,y)
                })
            }
            else if (droppedObject.hasClass('obj')) {
                model.issueCommand('top.driver.workflow.add("'+droppedPath+'")')
            }
        }
    });
    
    // ask model for an update whenever something changes
    model.addListener(update)

    /** update workflow diagram */
    function updateWorkflow(json) {
        workflow.clear()
        comp_figs = {}
        flow_figs = {}
        if (Object.keys(json).length > 0) {
            updateFigures('',json)
            //for (fig in flow_figs) // quick & dirty way to fully contain sub-workflows
                resizeFlowFigures()
        }
    }
    
    /** expand workflow (container) figures to contain all their children */
    function resizeFlowFigures() {
        var count,figWidth = 100,figHeight = 50
        for (fig in flow_figs) {
            var i=0, w=0, h=0,
                children = flow_figs[fig].getChildren()
            for (i=0;i<children.size;i++) {
                w = w+children.get(i).getWidth() +20
                h = h+children.get(i).getHeight()+20
            }
            flow_figs[fig].setDimension(w+20,h+20)
        }
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
    
    /** get the pathname of the parent component */
    function getParentPath(path) {
        parent_path = ''
        if (path) {
            var lastdot = path.lastIndexOf('.')
            if (lastdot > 0)
                parent_path = path.substring(0,lastdot)
        }
        return parent_path
    }
    
    /** update workflow by recreating figures from JSON workflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(flow_name,json) {
        var path = json['pathname'],
            type = json['type'],
            drvr = json['driver'],
            flow = json['workflow'],
            asm  = getParentPath(path),
            fig, coords
        
        if (flow) {
            // add driver figure
            fig = new openmdao.ComponentFigure(model,path,type)
            coords = getNextCoords()
            comp_figs[path] = fig
            if (flow_figs[flow_name])
                flow_figs[flow_name].addChild(fig)
            workflow.addFigure(fig,coords['x']+20,coords['y']+20)

            // add workflow compartment figure
            fig = new openmdao.WorkflowFigure(model,asm)
            coords = getNextCoords()
            workflow.addFigure(fig,coords['x'],coords['y'])
            if (flow_figs[flow_name])
                flow_figs[flow_name].addChild(fig)
            flow_figs[asm] = fig

            jQuery.each(flow,function(idx,comp) {
                updateFigures(asm,comp)
            })
        }
        else if (drvr) {
            // don't add a figure for an assembly, it will be represented by it's driver
            updateFigures(asm,drvr)
        }
        else {
            // add component figure
            fig = new openmdao.ComponentFigure(model,path,type)
            coords = getNextCoords()
            comp_figs[path] = fig
            if (flow_figs[asm]) {
                workflow.addFigure(fig,coords['x']+20,coords['y']+20)
                flow_figs[asm].addChild(fig)
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
        model.getWorkflow(updateWorkflow, function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error getting workflow (status="+jqXHR.status+"): "+jqXHR.statusText)
                openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting workflow',600,400)
                debug.error(jqXHR)
            })
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)

    /***********************************************************************
     *  privileged
     ***********************************************************************/
        
}

/** set prototype */
openmdao.WorkflowDiagram.prototype = new openmdao.BasePane();
openmdao.WorkflowDiagram.prototype.constructor = openmdao.WorkflowDiagram;
