
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.StructureDiagram = function(id,model,pathname) {
    openmdao.StructureDiagram.prototype.init.call(this,id,'Structure: '+pathname,[]);
    
    // initialize private variables
    var self = this,
        pane = new openmdao.StructurePane(jQuery('#'+id),model,pathname,'Data',false);
        
    /** update the schematic with data from the model */
    function update() {
        model.getStructure(pane.pathname, 
                           pane.loadData, 
                           function(jqXHR, textStatus, errorThrown) {
                               pane.pathname = ''
                               debug.error("Error getting Structure (status="+jqXHR.status+"): "+jqXHR.statusText)
                               debug.error('jqXHR:',jqXHR)
                           })
    };
    
    // ask model for an update whenever something changes
    model.addListener('',update)
    
    /** set the pathname for which to display the Structure */
    this.showStructure = function(path) {        
        if (pane.pathname !== path) {
            // if not already showing Structure for this pathname
            pane.pathname = path;            
            self.setTitle('Structure: '+path);
            update();
        }
    };
    
    /** get the pathname for the current structure */
    this.getPathname = function() {
        return pane.pathname;
    };
}

/** set prototype */
openmdao.StructureDiagram.prototype = new openmdao.BaseFrame();
openmdao.StructureDiagram.prototype.constructor = openmdao.StructureDiagram;
