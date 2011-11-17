
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowPane = function(elm,model,pathname,name,editable) {
    // initialize private variables
    var self = this,
        figures = {},
        dataflowID = "#"+pathname.replace(/\./g,'-')+"-dataflow",
        dataflowCSS = 'height:'+(screen.height-100)+'px;width:'+(screen.width-100)+'px;overflow:auto;'
        dataflowDiv = jQuery('<div id='+dataflowID+' style="'+dataflowCSS+'">').appendTo(elm),
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
                y = Math.round(ui.offset.top - off.top);
            var elem = dataflowDiv[0];
            var zindex = document.defaultView.getComputedStyle(elem,null).getPropertyValue("z-index");
            debug.info(droppedName,'dropped on dataflow:',self.pathname,'z-index',dataflowDiv.css('z-index'),'zIndex',dataflowDiv.css('zIndex'));
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForValue('Specify a name for the new '+droppedName,function(name) {
                    model.addComponent(droppedPath,name,self.pathname)
                })
            }
        }
    });
    
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
            
    /** update dataflow diagram */
    this.loadData = function(json) {
        dataflow.clear()
        figures = {}
        if (Object.keys(json).length > 0) {
            updateFigures(json,false)
        }
    }
  
}
