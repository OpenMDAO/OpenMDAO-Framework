
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFrame = function(id,model,pathname) {
    openmdao.WorkflowFrame.prototype.init.call(this,id,'Workflow: '+pathname,[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pane = new openmdao.WorkflowPane(jQuery('#'+id),model,pathname,'Workflow');

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the model */
    this.update = function() {
        model.getWorkflow(pane.pathname,
                          pane.loadData,
                          function(jqXHR, textStatus, errorThrown) {
                              pane.pathname = '';
                              debug.error("Error getting workflow (status="+jqXHR.status+"): "+jqXHR.statusText);
                              debug.error('jqXHR:',jqXHR);
                          });
    };

    /** set the pathname of the object for which to display the workflow */
    this.showWorkflow = function(path) {
        if (pane.pathname !== path) {
            // if not already showing workflow for this pathname
            pane.pathname = path;
            self.setTitle('Workflow: '+path);
            this.update();
        }
    };

    /** get the pathname for the current workflow */
    this.getPathname = function() {
        return pane.pathname;
    };

    // ask model for an update whenever something changes
    model.addListener('',this.update);
};

/** set prototype */
openmdao.WorkflowFrame.prototype = new openmdao.BaseFrame();
openmdao.WorkflowFrame.prototype.constructor = openmdao.WorkflowFrame;
