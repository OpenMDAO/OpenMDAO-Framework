
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowDiagram = function(id,model,pathname) {
    openmdao.DataflowDiagram.prototype.init.call(this,id,'Structure: '+pathname,[]);
    
    // initialize private variables
    var self = this,
        pane = new openmdao.DataflowPane(jQuery('#'+id),model,pathname,'Data',false);
        
    /** update the schematic with data from the model */
    function update() {
        model.getStructure(pane.pathname, 
                           pane.loadData, 
                           function(jqXHR, textStatus, errorThrown) {
                               pane.pathname = ''
                               debug.error("Error getting dataflow (status="+jqXHR.status+"): "+jqXHR.statusText)
                               debug.error('jqXHR:',jqXHR)
                           })
    };
    
    // ask model for an update whenever something changes
    model.addListener('',update)
    
    /** set the pathname for which to display the dataflow */
    this.showDataflow = function(path) {        
        if (pane.pathname !== path) {
            // if not already showing dataflow for this pathname
            pane.pathname = path;            
            self.setTitle('Dataflow: '+path);
            update();
        }
    };
    
    /** get the pathname for the current structure */
    this.getPathname = function() {
        return pane.pathname;
    };
}

/** set prototype */
openmdao.DataflowDiagram.prototype = new openmdao.BaseFrame();
openmdao.DataflowDiagram.prototype.constructor = openmdao.DataflowDiagram;
