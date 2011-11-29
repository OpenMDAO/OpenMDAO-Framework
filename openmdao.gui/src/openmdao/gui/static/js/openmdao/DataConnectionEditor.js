
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
        if (!data || !data['outputs'] || !data['inputs']) {
            // don't have what we need, probably something got deleted
            self.close();
        }
        else {
            dataflow.clear();
            figures = {};
            var x = 20, y = 10;
            jQuery.each(data['outputs'], function(idx,outvar) {
                var src_name = src_comp+'.'+outvar['name'],
                    src_path = self.pathname+'.'+src_name,
                    fig = new openmdao.DataflowVariableFigure(model,src_path,outvar,'output');
                dataflow.addFigure(fig,x,y);
                figures[src_name] = fig
                y = y + fig.height + 10;
            });
            x = 250, y = 10;
            jQuery.each(data['inputs'], function(idx,invar) {
                var dst_name = dst_comp+'.'+invar['name'],
                    dst_path = self.pathname+'.'+dst_name,
                    fig = new openmdao.DataflowVariableFigure(model,dst_path,invar,'input');
                dataflow.addFigure(fig,x,y);
                figures[dst_name] = fig
                y = y + fig.height + 10;
            });
            dataflowDiv.css({'height':y+'px','width': x+100+'px'});
            
            jQuery.each(data['connections'],function(idx,conn) {
                // internal connections
                if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
                    var src_name = conn[0],
                        dst_name = conn[1],
                        src_fig = figures[src_name],
                        dst_fig = figures[dst_name],
                        src_port = src_fig.getPort("output"),
                        dst_port = dst_fig.getPort("input");                        
                    c = new draw2d.Connection()
                    c.setSource(src_port);
                    c.setTarget(dst_port);
                    c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                    c.setRouter(new draw2d.BezierConnectionRouter());
                    c.getContextMenu=function(){
                        var menu=new draw2d.Menu();
                        var oThis=this;
                        menu.appendMenuItem(new draw2d.MenuItem("Disconnect",null,function(){
                                var asm = 'top.'+self.pathname,
                                    cmd = asm + '.disconnect("'+src_name+"','"+dst_name+'");'
                                        + asm + '.config_changed(update_parent=True);';
                                model.issueCommand(cmd);
                            })
                        );
                        return menu;
                    };
                    dataflow.addFigure(c);
                    src_port.setBackgroundColor(new draw2d.Color(0,0,0));
                    dst_port.setBackgroundColor(new draw2d.Color(0,0,0));
                }
                // TODO: handle connections to parent assembly vars (e.g. Vehicle.velocity)
                // TODO: show passthroughs somehow
            })
        }
    }
    
    /** if there is something loaded, update it from the model */
    function update() {
        self.editConnections(self.pathname,self.src_comp,self.dst_comp);
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