
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowPane = function(elm, project, pathname, name) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        workflowID = pathname.replace(/\./g,'-')+'-workflow',
        workflowCSS = 'min-height:'+screen.height+'px;'+
                      'min-width:'+screen.width+'px;'+
                      'position:relative;' +
                      'border: 0px',
        workflow = jQuery('<div id='+workflowID+' style="'+workflowCSS+'">')
            .appendTo(elm),
        flows = {},
        flow_order = [];  // Tracks order for consistent redraw.

    this.pathname = pathname;

    elm.css({ 'overflow':'auto' });
    workflow.css({ 'background-image': 'url("/static/images/grid_10.png")' });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the project */
    this.showWorkflow = function(pathname) {
        self.pathname = pathname;
        project.getWorkflow(self.pathname)
            .done(self.loadData)
            .fail(function(jqXHR, textStatus, err) {
                debug.error("Error getting workflow for", self.pathname,
                            jqXHR, textStatus, err);
                self.pathname = '';
            });
    };

    /** update workflow diagram */
    this.loadData = function(json) {
        var drawnFlows = [],
            updated_flows = [],
            deleted_flows = [];

        // We may get a single workflow object or a list of workflow objects
        // if we get a single workflow, stick it in a list for consistency
        if (!jQuery.isArray(json)) {
            json = [json];
        }

        // build lists of updated and deleted flows
        jQuery.each(json, function(idx, flow) {
            if (flow_order.indexOf(flow.pathname) >= 0) {
                updated_flows.push(flow.pathname);
            }
        });
        jQuery.each(flow_order, function(idx, name) {
            if (updated_flows.indexOf(name) < 0) {
                deleted_flows.push(name);
            }
        });

        // redraw updated flows in same order, remove deleted flows
        jQuery.each(flow_order, function(idx, name) {
            if (updated_flows.indexOf(name) >= 0) {
                jQuery.each(json, function(idx, flow) {
                    if (flow.pathname === name) {
                        flows[name].update(flow);
                        drawnFlows.push(name);
                    }
                });
            }
            else {
                flows[name].destroy();
                delete flows[name];
            }
        });

        // draw new flows
        jQuery.each(json, function(idx, flow) {
            if (drawnFlows.indexOf(flow.pathname) < 0) {
                flows[flow.pathname] = new openmdao.WorkflowFigure(workflow, project, '', flow);
                drawnFlows.push(flow.pathname);
            }
        });

        flow_order = drawnFlows;
    };

};
