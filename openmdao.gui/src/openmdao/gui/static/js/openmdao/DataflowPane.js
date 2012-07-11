
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
                      'overflow:auto;',
        dataflowDiv = jQuery('<div id='+dataflowID+' style="'+dataflowCSS+'">')
                      .appendTo(elm),
        dataflow = new draw2d.Workflow(dataflowID),
        dataflowFig = null;

    dataflow.setBackgroundImage( "/static/images/grid_10.png", true);

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
                bestfig = dataflow.getBestCompartmentFigure(x,y);
            var elem = dataflowDiv[0];
            var zindex = document.defaultView.getComputedStyle(elem,null)
                         .getPropertyValue("z-index");
            debug.info(droppedName,'(modpath=',droppedPath,') ',
                       'dropped on dataflow:',self.pathname,
                       'z-index',dataflowDiv.css('z-index'),
                       'zIndex',dataflowDiv.css('zIndex'));
            if (droppedObject.hasClass('objtype')) {
                openmdao.Util.promptForValue('Enter name for new '+droppedName,
                    function(name) {
                        if (bestfig) {
                            model.addComponent(droppedPath,name,bestfig.pathname);
                        }
                        else {
                            model.addComponent(droppedPath,name,self.pathname);
                        }
                    }
                );
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
