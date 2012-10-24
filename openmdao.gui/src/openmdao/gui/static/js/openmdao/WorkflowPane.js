
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowPane = function(elm,model,pathname,name) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        workflowID = pathname.replace(/\./g,'-')+'-workflow',
        workflowCSS = 'height:'+(screen.height-100)+'px;'+
                      'width:'+(screen.width-100)+'px;'+
                      'position:relative;' +
                      'border: 0px',
        workflow = jQuery('<div id='+workflowID+' style="'+workflowCSS+'">')
            .appendTo(elm),
        roots = [];  // Tracks order for consistent redraw.

    this.pathname = pathname;

    elm.css({ 'overflow':'auto' });
    workflow.css({ 'background-image': 'url("/static/images/grid_10.png")' });

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
        workflow.html('');

        var drawnFlows = [];

        function draw(flow) {
           var fig = new openmdao.WorkflowFigure(workflow, model, '', flow);
           drawnFlows.push(flow.pathname);
            // give browser a few ms to reflow everything then resize background
            setTimeout(function(){
                fig.getElement().find('.WorkflowFigure').trigger('setBackground');
            },1000);
        }

        // Redraw existing flows in same order.
        jQuery.each(roots, function(idx, name) {
            jQuery.each(json, function(idx, flow) {
                if (flow.pathname === name) {
                    draw(flow);
                }
            });
        });

        // Draw new flows.
        jQuery.each(json, function(idx, flow) {
            if (drawnFlows.indexOf(flow.pathname) < 0) {
                draw(flow);
            }
        });

        roots = drawnFlows;
    };

};
