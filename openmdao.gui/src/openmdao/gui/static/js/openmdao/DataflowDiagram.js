
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowDiagram = function(id,model,pathname) {
    openmdao.DataflowDiagram.prototype.init.call(this,id,'Dataflow: '+pathname,[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pane = new openmdao.DataflowPane(jQuery('#'+id),model,pathname,'Data',false);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the model */
    this.update = function() {
        model.getDataflow(pane.pathname, 
                           pane.loadData, 
                           function(jqXHR, textStatus, errorThrown) {
                               pane.pathname = ''
                               debug.error("Error getting dataflow (status="+jqXHR.status+"): "+jqXHR.statusText)
                               debug.error('jqXHR:',jqXHR)
                           })
    };

    /** set the pathname for which to display the dataflow */
    this.showDataflow = function(path) {
        if (pane.pathname !== path) {
            // if not already showing dataflow for this pathname
            pane.pathname = path;
            self.setTitle('Dataflow: '+path);
            this.update();
        }
    };

    /** get the pathname for the current dataflow */
    this.getPathname = function() {
        return pane.pathname;
    };

    // ask model for an update whenever something changes
    model.addListener('',this.update);
}

/** set prototype */
openmdao.DataflowDiagram.prototype = new openmdao.BaseFrame();
openmdao.DataflowDiagram.prototype.constructor = openmdao.DataflowDiagram;
