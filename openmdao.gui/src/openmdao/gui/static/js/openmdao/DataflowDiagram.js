
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowDiagram = function(id,model,pathname) {
    openmdao.DataflowDiagram.prototype.init.call(this,id,'Dataflow: '+pathname,[]);
    
    // initialize private variables
    var self = this,
        pane = new openmdao.DataflowPane(jQuery('#'+id),model,pathname,'Data',false);
        
    self.pathname = pathname;
    
    /** update the schematic with data from the model */
    function update() {
        model.getDataflow(self.pathname, 
                          pane.loadData, 
                          function(jqXHR, textStatus, errorThrown) {
                              self.pathname = ''
                              alert("Error getting dataflow (status="+jqXHR.status+"): "+jqXHR.statusText)
                              openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting dataflow',600,400)
                              debug.error(jqXHR)
                          })
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)
    
    /** set the pathname for which to display the dataflow */
    this.showDataflow = function(path) {        
        if (self.pathname !== path) {
            // if not already showing dataflow for this pathname
            self.pathname = path;
            self.setTitle('Dataflow: '+path);
            update();
        }
    }
}

/** set prototype */
openmdao.DataflowDiagram.prototype = new openmdao.BaseFrame();
openmdao.DataflowDiagram.prototype.constructor = openmdao.DataflowDiagram;
