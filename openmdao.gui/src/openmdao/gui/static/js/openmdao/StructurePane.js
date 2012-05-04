
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.StructurePane = function(elm,model,pathname,name,editable) {
    // initialize private variables
    var self = this,
        figures = {},
        structureID = pathname.replace(/\./g,'-')+"-structure",
        structureCSS = 'height:'+(screen.height-100)+'px;width:'+(screen.width-100)+'px;overflow:auto;',
        structureDiv = jQuery('<div id='+structureID+' style="'+structureCSS+'">').appendTo(elm),
        structure = new draw2d.Workflow(structureID);
        
    self.pathname = pathname;
    
    structure.setBackgroundImage( "/static/images/grid_10.png", true);
        
    // make the structure pane droppable
    structureDiv.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("path"),
                off = structureDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top);
            var elem = structureDiv[0];
            var zindex = document.defaultView.getComputedStyle(elem,null).getPropertyValue("z-index");            
            debug.info(droppedName,'(path=',droppedPath,') dropped on structure:',self.pathname,'z-index',structureDiv.css('z-index'),'zIndex',structureDiv.css('zIndex'));
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForValue('Specify a name for the new '+droppedName,function(name) {
                    model.addComponent(droppedPath,name,self.pathname);
                })
            };
        }
    });
    
    /** update structure by recreating figures from JSON structure data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json['components'],function(idx,comp) {
            var name = comp['name'],
                type = comp['type'],
                valid = comp['valid'],
                fig = figures[name];
                
            if (!fig) {
                if (self.pathname) {
                    var fig = new openmdao.StructureComponentFigure(model,self.pathname+'.'+name,type,valid);
                }
                else {
                    var fig = new openmdao.StructureComponentFigure(model,name,type,valid);
                }
                fig.setTitle(name);
                figures[name] = fig;
            };
            
            fig.setContent('<center>(('+type+'))'+'</center>');
            
            var count = Object.keys(figures).length,
                x = (count-1)*(fig.getWidth()+20)  + 20,
                y = (count-1)*(fig.getHeight()+20) + 20;
            structure.addFigure(fig,x,y);
        })
        
        jQuery.each(json['connections'],function(idx,conn) {
            // internal connections only
            if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
                var src_name = conn[0].split('.')[0],
                    dst_name = conn[1].split('.')[0],
                    src_fig = figures[src_name],
                    dst_fig = figures[dst_name];
                    c = new openmdao.ContextMenuConnection();
                // TODO: only create new connection if one doesn't already exist
                c.setSource(src_fig.getPort("output"));
                c.setTarget(dst_fig.getPort("input"));
                c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                c.onDoubleClick = function() {
                    new openmdao.DataConnectionEditor(model,self.pathname,src_name,dst_name);
                };
                structure.addFigure(c);
            }
        })
        
        layout();
    }

    /** layout component figures */
    function layout() {
        var connected = [],
            unconnected = [],
            i=0, x=20, y=20;

        jQuery.each(figures, function(idx,fig) {
            if (fig.isConnected()) {
                connected.push(fig);
            }
            else {
                unconnected.push(fig);
            }
        });

        // unconnected components are laid out in rows
        var row = 0,
            row_start = 0,
            max_width = structure.getWidth();
        
        jQuery.each(unconnected,function(idx,fig) {
            x = (idx-row_start)*(fig.getWidth()+20) + 20;
            if ((x + fig.getWidth()) > max_width) {
                row = row + 1;
                row_start = idx;
                x = (idx-row_start)*(fig.getWidth()+20) + 20;
                y = y + row*(fig.getHeight()+20);
            }
            fig.setPosition(x,y);
        });

        // connected components are laid out diagonally 
        // (top left to bottom right)
        x = 0;
        jQuery.each(connected,function(idx,fig) {
            x = idx*(fig.getWidth()+20) + 20;
            y = y + (fig.getHeight()+20) + 20;
            fig.setPosition(x,y);
        });
    };

    /** update structure diagram */
    this.loadData = function(json) {
        structure.clear();
        figures = {};
        if (Object.keys(json).length > 0) {
            updateFigures(json,false);
        };
    }
  
}
