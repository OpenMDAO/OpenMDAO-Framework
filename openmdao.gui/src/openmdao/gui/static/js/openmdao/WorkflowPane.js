
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowPane = function(elm,model,pathname,name) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        workflowID = "#"+pathname.replace(/\./g,'-')+"-workflow",
        workflowCSS = 'height:'+(screen.height-100)+'px;'+
                      'width:'+(screen.width-100)+'px;'+
                      'position:relative;',
        workflowDiv = jQuery('<div id='+workflowID+' style="'+workflowCSS+'">')
                      .appendTo(elm),
        workflow = new draw2d.Workflow(workflowID),
        comp_figs = {},
        flow_figs = {},
        roots = [];  // Tracks order for consistent redraw.

    this.pathname = pathname;

    workflow.setBackgroundImage( "/static/images/grid_10.png", true);
    elm.css({ 'overflow':'auto' });
    workflow.setViewPort(elm.attr('id'));

    /** expand workflow (container) figures to contain all their children */
    function resizeFlowFigures() {
        var xmax = 0;
        jQuery.each(flow_figs, function (flowpath,flowfig) {
            flowfig.resize();
            xmax = Math.max(xmax, flowfig.getAbsoluteX()+flowfig.getWidth());
        });
        return xmax;
    }

    /** update workflow from JSON workflow data
     */
    function updateFigures(flow_name, json, offset) {
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
                workflow.addFigure(comp_fig, offset+50, 50);
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

            jQuery.each(flow, function(idx, comp) {
                updateFigures(flowpath, comp, offset);
            });
        }
        else if (drvr) {
            // don't add a figure for an assembly,
            // it will be represented by it's driver
            updateFigures(flow_name, drvr, offset);
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
                workflow.addFigure(comp_fig, offset+50, 50);
            }
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

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
        // Where does non-Array come from? Occurs during drag-n-drop test.
        if (!jQuery.isArray(json)) {
            json = [json];
        }
        workflow.clear();
        comp_figs = {};
        flow_figs = {};
        var offset = 0,
            drawnFlows = [];
            draw = function(flow, offset) {
                       updateFigures('', flow, offset);
                       xmax = resizeFlowFigures();
                       drawnFlows.push(flow.pathname);
                       return xmax;
                   };

        // Redraw existing flows in same order.
        jQuery.each(roots, function(idx, name) {
            jQuery.each(json, function(idx, flow) {
                if (flow.pathname === name) {
                    offset = draw(flow, offset);
                }
            });
        });

        // Draw new flows.
        jQuery.each(json, function(idx, flow) {
            if (drawnFlows.indexOf(flow.pathname) < 0) {
                offset = draw(flow, offset);
            }
        });

        roots = drawnFlows;
    };

};
