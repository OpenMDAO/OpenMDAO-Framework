
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowPane = function(elm,model,pathname,name) {
    // initialize private variables
    var self = this,
        comp_figs = {},
        flow_figs = {},
        workflowID = "#"+pathname.replace(/\./g,'-')+"-workflow",
        workflowCSS = 'height:'+(screen.height-100)+'px;'+
                      'width:'+(screen.width-100)+'px;' +
                      'overflow:auto;',
        workflowDiv = jQuery('<div id='+workflowID+' style="'+workflowCSS+'">')
                      .appendTo(elm),
        workflow = new draw2d.Workflow(workflowID);

    self.pathname = pathname;

    workflow.setBackgroundImage( "/static/images/grid_10.png", true);

    // make the workflow pane droppable, handle drops of objtype
    // (obj drops from ComponentTree are handled in ComponentTreeFrame.js)
    workflowDiv.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) {
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("modpath"),
                off = workflowDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top),
                flowfig = workflow.getBestCompartmentFigure(x,y),
                bestfig = workflow.getBestFigure(x,y);
            debug.info(droppedName,'dropped on workflow',self.pathname,bestfig);
            if (droppedObject.hasClass('objtype')) {
                if (bestfig instanceof openmdao.WorkflowFigure) {
                    var parent = openmdao.Util.getPath(bestfig.pathname),
                        prompt = 'Specify a name for the new '+droppedName+'<br>'+
                                 '(It will be added to '+parent+' and to <br>'+
                                 'the workflow of '+ bestfig.pathname+')';
                    openmdao.Util.promptForValue(prompt, function(name) {
                            model.addComponent(droppedPath,name,parent, function() {
                                // if successful, then add to workflow as well
                                cmd = bestfig.pathname+'.workflow.add("'+name+'")';
                                model.issueCommand(cmd);
                            });
                        }
                    );
                }
                else if ((/^openmdao.lib.drivers./).test(droppedPath)) {
                    // TODO: need interface info to check if type & fig are drivers
                    if (bestfig instanceof openmdao.WorkflowComponentFigure &&
                        openmdao.Util.getName(bestfig.pathname) === 'driver') {
                        path = openmdao.Util.getPath(bestfig.pathname);
                        // TODO: need a 'replace' function to preserve driver config
                        model.addComponent(droppedPath,'driver',path);
                    }
                    else {
                        debug.info(droppedPath,'was not dropped on a driver', bestfig);
                    }
                }
                else {
                   debug.info('fell through and did nothing!');
                   debug.info('bestfig');
                   debug.info(bestfig);
                }
            }
            else {
                debug.info('Workflow drop was not valid (obj on flow or objtype on driver)');
            }
        }
    });

    /** expand workflow (container) figures to contain all their children */
    function resizeFlowFigures() {
        jQuery.each(flow_figs, function (flowpath,flowfig) {
            flowfig.resize();
        });
    }

    /** update workflow from JSON workflow data
     */
    function updateFigures(flow_name,json) {
        var path  = json.pathname,
            type  = json.type,
            valid = json.valid,
            drvr  = json.driver,
            flow  = json.workflow,
            asm   = openmdao.Util.getPath(path),
            comp_key = flow_name+':'+path,
            comp_fig, flow_fig, flowpath, newflow_fig, count, x, y;

        if (flow) {
            // add driver figure
            if (comp_figs.hasOwnProperty(comp_key)) {
                comp_fig = comp_figs[comp_key];
            }
            else {
                comp_fig = new openmdao.WorkflowComponentFigure(model,path,type,valid);
                comp_figs[comp_key] = comp_fig;
            }

            flow_fig = flow_figs[flow_name];
            if (flow_fig) {
                flow_fig.addComponentFigure(comp_fig);
            }
            else {
                workflow.addFigure(comp_fig,50,50);
            }

            // add workflow compartment figure for this flow
            // (overlap bottom right of driver figure)
            flowpath = flow_name+'.'+path;
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
            });
        }
        else if (drvr) {
            // don't add a figure for an assembly,
            // it will be represented by it's driver
            updateFigures(flow_name,drvr);
        }
        else {
            // add component figure
            if (comp_figs.hasOwnProperty(comp_key)) {
                comp_fig = comp_figs[comp_key];
            }
            else {
                comp_fig = new openmdao.WorkflowComponentFigure(model,path,type,valid);
                comp_figs[comp_key] = comp_fig;
            }

            flow_fig = flow_figs[flow_name];
            if (flow_fig) {
                flow_fig.addComponentFigure(comp_fig);
            }
            else {
                workflow.addFigure(comp_fig,50,50);
            }
        }
    }

    /** update the schematic with data from the model */
    this.showWorkflow = function(pathname) {
        self.pathname = pathname;
        model.getWorkflow(self.pathname,
                          self.loadData,
                          function(jqXHR, textStatus, errorThrown) {
                              self.pathname = '';
                              debug.error("Error getting workflow ", jqXHR);
                          });
    };

    /** update workflow diagram */
    this.loadData = function(json) {
        workflow.clear();
        comp_figs = {};
        flow_figs = {};
        if (Object.keys(json).length > 0) {
            updateFigures('',json);
            resizeFlowFigures();
        }
    };

};
