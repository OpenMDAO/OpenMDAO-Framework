
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.WorkflowDiagram = function(id,model,pathname) {
    openmdao.WorkflowDiagram.prototype.init.call(this,id,'Workflow: '+pathname,[]);
    
    // initialize private variables
    var self = this,
        pane = new openmdao.WorkflowPane(jQuery('#'+id),model,pathname,'Workflow',false);
        
    self.pathname = pathname;
        
    /** update the schematic with data from the model */
    function update() {
        model.getWorkflow(self.pathname, 
                          pane.loadData, 
                          function(jqXHR, textStatus, errorThrown) {
                              self.pathname = ''
                              debug.error("Error getting workflow (status="+jqXHR.status+"): "+jqXHR.statusText)
                              debug.error('jqXHR:',jqXHR)
                          })
    };
    
    // ask model for an update whenever something changes
    model.addListener(update);
    
    /** set the pathname of the object for which to display the workflow */
    this.showWorkflow = function(path) {        
        if (self.pathname !== path) {
            // if not already editing this object, create the tabbed panes
            self.pathname = path;
            self.setTitle('Workflow: '+path);
            update();
        };
    };
    
    /** get the pathname for the current workflow */
    this.getPathname = function() {
        return self.pathname;
    };
}

/** set prototype */
openmdao.WorkflowDiagram.prototype = new openmdao.BaseFrame();
openmdao.WorkflowDiagram.prototype.constructor = openmdao.WorkflowDiagram;
