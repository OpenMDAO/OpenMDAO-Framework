
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataConnectionEditor = function(model,pathname,src_comp,dst_comp) {
    var id = ('DCE-'+pathname+'-'+src_comp+'-'+dst_comp).replace(/\./g,'-');
    openmdao.DataConnectionEditor.prototype.init.call(this, id,
        'Connections: '+openmdao.Util.getName(pathname)+' '+src_comp+' to '+dst_comp);
    
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
    self.src_comp = src_comp;
    self.dst_comp = dst_comp;

    dataflowDiv.css({'background':'gray'});
    //dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
    
    model.addListener(update)

    function loadData(data) {
        debug.info('connection editor:',self.pathname,self.src_comp,self.dst_comp)
        debug.info('connections:',data)
        if (!data || !data['outputs'] || !data['inputs']) {
            // don't have what we need, probably something got deleted
            self.close();
        }
        else {
            dataflow.clear();
            figures = {};
            var x = 20, y = 10;
            jQuery.each(data['outputs'], function(idx,outvar) {
                var varPath = self.pathname+'.'+src_comp+'.'+outvar,
                    fig = new openmdao.DataflowVariableFigure(model,varPath,'output');
                dataflow.addFigure(fig,x,y);
                figures[src_comp+'.'+outvar] = fig
                y = y + 60;
            });
            x = 250, y = 10;
            jQuery.each(data['inputs'], function(idx,invar) {
                var varPath = self.pathname+'.'+dst_comp+'.'+invar,
                    fig = new openmdao.DataflowVariableFigure(model,varPath,'input');
                dataflow.addFigure(fig,x,y);
                figures[dst_comp+'.'+invar] = fig
                y = y + 60;
            });
            dataflowDiv.css({'height':y+'px','width': x+100+'px'});
            
            jQuery.each(data['connections'],function(idx,conn) {
                // internal connections
                if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
                    var src_name = conn[0],
                        dst_name = conn[1],
                        src_fig = figures[src_name],
                        dst_fig = figures[dst_name];
                        c = new openmdao.ContextMenuConnection()
                    c.setSource(src_fig.getPort("output"));
                    c.setTarget(dst_fig.getPort("input"));
                    c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                    dataflow.addFigure(c);
                }
                // TODO: handle connections to parent assembly vars (e.g. Vehicle.velocity)
                // TODO: show passthroughs somehow
            })
        }
    }
    
    /** if there is something loaded, update it from the model */
    function update() {
        if (self.pathname) {
            self.editConnections(self.pathname,self.src_comp,self.dst_comp);
        }
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** edit connectiosn between the source and destination objects in the assembly */
    this.editConnections = function(pathname, src_comp, dst_comp) {        
        self.pathname = pathname;
        self.src_comp = src_comp;
        self.dst_comp = dst_comp;
        
        var callback = loadData;
        model.getConnections(pathname, src_comp, dst_comp, callback,
            function(jqXHR, textStatus, errorThrown) {
                debug.error(jqXHR,textStatus,errorThrown);
                self.close();                
            }
        )
        return this;
    }

    update();

}

/** set prototype */
openmdao.DataConnectionEditor.prototype = new openmdao.BaseFrame();
openmdao.DataConnectionEditor.prototype.constructor = openmdao.DataConnectionEditor;