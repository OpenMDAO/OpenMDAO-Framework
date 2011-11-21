
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.SlotsPane = function(elm,model,pathname,name,editable) {
    // initialize private variables
    var self = this,
        figures = {},
        slotsID = "#"+pathname.replace(/\./g,'-')+"-slots",
        slotsCSS = 'overflow:auto;',
        slotsDiv = jQuery('<div id='+slotsID+' style="'+slotsCSS+'">').appendTo(elm),
        slots = new draw2d.Workflow(slotsID);
        
    self.pathname = pathname;
    
    //slots.setBackgroundImage( "/static/images/grid_10.png", true);
        
    // make the slots pane droppable
    slotsDiv.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("path"),
                off = slotsDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top);
            var elem = slotsDiv[0];
            var zindex = document.defaultView.getComputedStyle(elem,null).getPropertyValue("z-index");
            debug.info(droppedName,'dropped on slots:',self.pathname,'z-index',slotsDiv.css('z-index'),'zIndex',slotsDiv.css('zIndex'));
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForValue('Specify a name for the new '+droppedName,function(name) {
                    model.addComponent(droppedPath,name,self.pathname)
                })
            }
        }
    });
    
    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            var name = slot['name'],
                type = slot['type'];
                
            if (self.pathname) {
                var fig = new openmdao.DataflowComponentFigure(model,self.pathname+'.'+name,type);
            }
            else {
                var fig = new openmdao.DataflowComponentFigure(model,name,type);
            }
                    
            fig.setTitle(name)
            figures[name] = fig
            fig.setContent('<center>(('+type+'))'+'</center>')
            // TODO: flexible grid layout (adjusting for size)
            var count = Object.keys(figures).length,
                x = (count-1)*(fig.getWidth()+20)  + 20,
                y = 20;
            //debug.info('count=',count,'x=',x,'y=',y)
            slots.addFigure(fig,x,y)            
        })

    }
            
    /** update slots diagram */
    this.loadData = function(json) {
        slots.clear()
        figures = {}
        if (Object.keys(json).length > 0) {
            updateFigures(json,false)
        }
    }
  
}
