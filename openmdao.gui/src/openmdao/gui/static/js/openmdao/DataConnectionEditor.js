
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataConnectionEditor = function(model,pathname,src_name,dst_name) {
    var id = ('DCE-'+pathname+'-'+src_name+'-'+dst_name).replace(/\./g,'-');
    openmdao.DataConnectionEditor.prototype.init.call(this, id,
        'Connections: '+src_name+' -> '+dst_name);
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this,
        figures = {},
        dataflowID = "#"+id+"-connections",
        dataflowDiv = jQuery('<div id='+dataflowID+'>').appendTo('<div style="background:white">').appendTo('#'+id),
        dataflow = new draw2d.Workflow(dataflowID);
        
    self.pathname = pathname;
    self.src_name = src_name;
    self.dst_name = dst_name;

    dataflowDiv.css({'background':'gray'});
    //dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
    
    model.addListener(update)

    function loadData(data) {
        debug.info('connection editor:',self.pathname,self.src_name,self.dst_name)
        debug.info('connections:',data)
        dataflow.clear();
        figures = {};
        var x = 20, y = 10;
        jQuery.each(data['outputs'], function(idx,outvar) {
            var fig = new openmdao.DataflowVariableFigure(model,self.pathname+'.'+outvar,'output');
            dataflow.addFigure(fig,x,y);
            y = y + 60;
        });
        x = 250, y = 10;
        jQuery.each(data['inputs'], function(idx,invar) {
            var fig = new openmdao.DataflowVariableFigure(model,self.pathname+'.'+invar,'input');
            dataflow.addFigure(fig,x,y);
            y = y + 60;
        });
        dataflowDiv.css({'height':y+'px','width': x+100+'px'});
        // TODO: draw in the connections & add handlers to add/rm connections
    }
    
    /** if there is something loaded, update it from the model */
    function update() {
        if (self.pathname) {
            self.editConnections(self.pathname,self.src_name,self.dst_name);
        }
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** edit connectiosn between the source and destination objects in the assembly */
    this.editConnections = function(pathname, src_name, dst_name) {        
        self.pathname = pathname;
        self.src_name = src_name;
        self.dst_name = dst_name;
        
        var callback = loadData;
        model.getConnections(pathname, src_name, dst_name, callback,
            function(jqXHR, textStatus, errorThrown) {
                openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting connections',600,400);
                debug.error(jqXHR,textStatus,errorThrown);
            }
        )
        return this;
    }

    update();

}

/** set prototype */
openmdao.DataConnectionEditor.prototype = new openmdao.BaseFrame();
openmdao.DataConnectionEditor.prototype.constructor = openmdao.DataConnectionEditor;