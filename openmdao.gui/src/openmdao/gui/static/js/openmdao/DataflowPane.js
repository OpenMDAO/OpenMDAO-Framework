
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DataflowPane = function(elm,model,pathname,name,editable) {
    // initialize private variables
    var self = this,
        dataflowID  = pathname.replace(/\./g,'-')+"-dataflow",
        dataflowCSS = 'height:'+(screen.height-100)+'px;'+
                      'width:'+(screen.width-100)+'px;'+
                      'overflow:auto;',
        dataflowDiv = jQuery('<div id='+dataflowID+' style="'+dataflowCSS+'">').appendTo(elm),
        dataflow = new draw2d.Workflow(dataflowID),
        dataflowFig;

    self.model = model;
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
            debug.info(droppedName,'(path=',droppedPath,') dropped on dataflow:',self.pathname,
                                   'z-index',dataflowDiv.css('z-index'),'zIndex',dataflowDiv.css('zIndex'));
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForValue('Specify a name for the new '+droppedName,function(name) {
                    model.addComponent(droppedPath,name,self.pathname);
                })
            };
        }
    });

    /** update dataflow diagram */
    this.loadData = function(json) {
        dataflow.clear();

        debug.info('DataflowPane.loadData',model,pathname,this);

        dataflowFig = new openmdao.DataflowFigure(self.model, self.pathname);
        dataflow.addFigure(dataflowFig,20,20);
        figures = {};
        if (Object.keys(json).length > 0) {
            dataflowFig.updateFigures(json);
        };
    }
}
