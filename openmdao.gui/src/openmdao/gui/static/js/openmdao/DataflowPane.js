
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DataflowPane = function(elm,model,pathname,name) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        dataflowID  = pathname.replace(/\./g,'-')+"-dataflow",
        dataflowCSS = 'height:'+(screen.height-100)+'px;'+
                      'width:'+(screen.width-100)+'px;'+
                      'position:relative;',
        dataflowDiv = jQuery('<div id='+dataflowID+' style="'+dataflowCSS+'">')
                      .appendTo(elm),
        dataflow = new draw2d.Workflow(dataflowID),
        dataflowFig = null;

    dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
    elm.css({ 'overflow':'auto' });
    dataflow.setViewPort(elm.attr('id'));

    // make the dataflow pane droppable
    dataflowDiv.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) {
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("modpath"),
                off = dataflowDiv.parent().offset(),
                x = Math.round(ui.offset.left - off.left),
                y = Math.round(ui.offset.top - off.top),
                bestFigure = dataflow.getBestCompartmentFigure(x,y);

            var elem = dataflowDiv[0];
            var zindex = document.defaultView.getComputedStyle(elem,null)
                         .getPropertyValue("z-index");
            debug.info(droppedName,'(modpath=',droppedPath,') ',
                       'dropped on dataflow:',self.pathname,
                       'z-index',dataflowDiv.css('z-index'),
                       'zIndex',dataflowDiv.css('zIndex'));
//            debug.info('ui.offset', ui.offset.left, ui.offset.top);
//            debug.info('offset', off.left, off.top);
//            debug.info('x, y', x, y);

            if (droppedObject.hasClass('objtype')) {
                var pathname = null,
                    addComp = true;
                if (bestFigure) {
                    pathname = bestFigure.pathname;
                    // maxmin is null for non-assembly components.
                    addComp = bestFigure.maxmin != ''
//                    debug.info('bestFigure', pathname, addComp);
//                    debug.info('dims', bestFigure.getX(), bestFigure.getY(),
//                               bestFigure.getWidth(), bestFigure.getHeight());
                }
                else {
                    pathname = self.pathname;
                    debug.info('me', pathname);
                }
                if (addComp) {
                    openmdao.Util.promptForValue('Enter name for new '+droppedName,
                        function(name) {
                            model.addComponent(droppedPath,name,pathname);
                        }
                    );
                }
                else {
                    openmdao.Util.confirm('Replace '+pathname+' with '+droppedName,
                        function() {
                            model.replaceComponent(pathname, droppedPath);
                        }
                    );
                }
            }
        }
    });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** change the dataflow to the one with the specified pathname */
    this.showDataflow = function(pathname) {
        self.pathname = pathname;
        self.update();
    };

    /** load json dataflow data */
    this.loadData = function(json) {
        // FIXME: just having it update itself for now, ignoring json data
        dataflowFig.updateDataflow(json);
//        this.update();
    };

    /** update by deleting existing dataflow and creating a new one */
    this.update = function() {
        if (dataflowFig !== null) {
            // need to minimize & destroy figures to get rid of listeners
            dataflowFig.minimize();
            dataflow.clear();
            dataflowFig.destroy();
        }
        dataflowFig = new openmdao.DataflowFigure(model, self.pathname);
        dataflow.addFigure(dataflowFig,20,20);
        dataflowFig.maximize();
    };

    this.showDataflow(pathname);
};
