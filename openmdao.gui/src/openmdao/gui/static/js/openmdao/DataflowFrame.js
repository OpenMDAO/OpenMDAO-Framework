
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowFrame = function(id,model,pathname) {
    openmdao.DataflowFrame.prototype.init.call(this,id,'Dataflow: '+pathname,[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pane = new openmdao.DataflowPane(jQuery('#'+id),model,pathname,'Dataflow');

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the model */
    this.update = function() {
        pane.update();
    };

    /** set the pathname of the object for which to display the dataflow */
    this.showDataflow = function(path) {
        if (pane.pathname !== path) {
            // if not already showing dataflow for this pathname
            self.setTitle('Dataflow: '+path);
            pane.pathname = path;
            pane.update();
        }
    };

    /** get the pathname for the current dataflow */
    this.getPathname = function() {
        return pane.pathname;
    };

    // ask model for an update whenever something changes
    model.addListener('',pane.update);
}

/** set prototype */
openmdao.DataflowFrame.prototype = new openmdao.BaseFrame();
openmdao.DataflowFrame.prototype.constructor = openmdao.DataflowFrame;
