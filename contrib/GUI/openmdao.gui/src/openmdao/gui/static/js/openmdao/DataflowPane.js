
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DataflowPane = function(elm, project, pathname, name, prop_fn) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        dataflowID  = pathname.replace(/\./g,'-')+"-dataflow",
        dataflowCSS = 'height:'+(screen.height*4)+'px;'+
                      'width:'+(screen.width*4)+'px;'+
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
    elm.droppable({
        accept: '.IComponent',
        tolerance: 'pointer',
        greedy: true,
        out: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOut(elm);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(elm);
        },
        drop: function(ev,ui) {
            var dropTarget = openmdao.drag_and_drop_manager.getDropTarget(ev, ui);
            if (dropTarget) {
                dropTarget.droppable('option', 'dropHandler')(ev, ui);
            }
        },
        dropHandler: function(ev,ui) {
            // could get same event multiple times if drop triggers for sibling targets
            if (this.dropEvent && this.dropEvent === ev.originalEvent) {
                return;  // already handled this drop event
            }
            this.dropEvent = ev.originalEvent;

            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("modpath");

            openmdao.drag_and_drop_manager.reset();
            openmdao.project.addObject(droppedPath, droppedName, self.pathname);
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
            dataflowFig.minimize(true);
            dataflow.removeFigure(dataflowFig);
            dataflowFig.destroy();
        }
        dataflowFig = new openmdao.DataflowFigure(project, self.pathname, prop_fn);
        dataflow.addFigure(dataflowFig,20,20);
        dataflowFig.maximize();
    };

    project.project_ready.always(function() {
        self.showDataflow(pathname);
    });
};
