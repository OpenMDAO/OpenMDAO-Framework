
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

    elm.css({ 'overflow':'auto' });
    dataflow.setViewPort(elm.attr('id'));
    dataflow.setBackgroundImage( "/static/images/grid_10.png", true);

    // dataflow pane is droppable only for the global dataflow (pathname === '')
    if (pathname === '') {
        /** Highlight this pane when it the cursor is over it and it can accept a drop */
        elm.highlightAsDropTarget=function() {
            dataflow.setBackgroundImage( "/static/images/grid_10_highlighted.png", true);
        };

        /** Turn off highlighting of this pane when it can no longer accept a drop */
        elm.unhighlightAsDropTarget=function() {
            dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
        };
    }

    /* Even though we only allow dropping on the global dataflow pane, we need
       to include all dataflow panes in the list of droppables so that handling
       of the layers works
    */
    elm.droppable ({
        accept: '.IComponent',
        out: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOut(elm);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(elm);
        },
        drop: function(ev,ui) {
            top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
            if (top_div) {
                var drop_function = top_div.droppable('option','actualDropHandler');
                drop_function(ev,ui);
            }
        },
        actualDropHandler: function(ev,ui) {
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("modpath");

            openmdao.drag_and_drop_manager.clearHighlightingDroppables() ;

            openmdao.Util.promptForValue('Enter name for new '+ droppedName, function(name) {
                model.addComponent(droppedPath,name,self.pathname);
            });
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
        dataflowFig.updateDataflow(json);
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

    model.model_ready.always(function() {
        self.showDataflow(pathname);
    });
};
