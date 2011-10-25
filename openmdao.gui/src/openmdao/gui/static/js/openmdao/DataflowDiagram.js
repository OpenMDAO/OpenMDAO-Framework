
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowDiagram = function(id,model,pathname) {
    openmdao.DataflowDiagram.prototype.init.call(this,id,'Dataflow: '+pathname,[]);
    
    // initialize private variables
    var self = this,
        figures = {},
        dataflowID = "#"+id+"-dataflow",
        dataflowDiv = jQuery('<div id='+dataflowID+' style="height:'+(screen.height-100)+'px;width:'+(screen.width-100)+'px">').appendTo('#'+id),
        dataflow = new draw2d.Workflow(dataflowID);
        
    self.pathname = pathname;
    
    dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
        
    // make the dataflow pane droppable
    dataflowDiv.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("path"),
                off = dataflowDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top)
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForName(function(name) { 
                    model.addComponent(droppedPath,name,x,y)
                })
            }
        }
    });
    
    /** update dataflow diagram */
    function updateDataflow(json) {
        dataflow.clear()
        figures = {}
        if (Object.keys(json).length > 0) {
            updateFigures(json,false)
        }
    }
  
    /** update dataflow by recreating figures from JSON dataflow data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json['components'],function(idx,comp) {
            var name = comp['name'],
                type = comp['type'];
                
            if (self.pathname) {
                var fig = new openmdao.DataflowComponentFigure(model,self.pathname+'.'+name,type);
            }
            else {
                var fig = new openmdao.DataflowComponentFigure(model,name,type);
            }
                    
            fig.setTitle(name)
            figures[name] = fig
            fig.setContent('<center>(('+type+'))'+'</center>')
            var count = Object.keys(figures).length,
                x = (count-1)*(fig.getWidth()+20)  + 20,
                y = (count-1)*(fig.getHeight()+20) + 20
            //debug.info('count=',count,'x=',x,'y=',y)
            dataflow.addFigure(fig,x,y)            
        })
        
        jQuery.each(json['connections'],function(idx,conn) {
            // internal connections only
            if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
                var src_name = conn[0].split('.')[0],
                    dst_name = conn[1].split('.')[0],
                    src_fig = figures[src_name],
                    dst_fig = figures[dst_name];
                    c = new openmdao.ContextMenuConnection()
                // TODO: only create new connection if one doesn't already exist
                c.setSource(src_fig.getPort("output"));
                c.setTarget(dst_fig.getPort("input"));
                c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                c.onDoubleClick = function() {
                    new openmdao.DataConnectionEditor(model,self.pathname,src_name,dst_name);
                };
                dataflow.addFigure(c);
            }
        })
    }
            
    /** update the schematic with data from the model */
    function update() {
        model.getDataflow(self.pathname, 
                          updateDataflow, 
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
