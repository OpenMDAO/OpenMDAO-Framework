
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

    /* used by the drag and drop code */
    var true_dropdiv = null;
    this.openmdao_model = model;
    this.dataflowDiv = dataflowDiv;

    dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
    elm.css({ 'overflow':'auto' });
    dataflow.setViewPort(elm.attr('id'));

    /* Even though we only allow dropping on the topmost dataflow, which has
       dataflowID of "-dataflow" ( which adds it to the globals ),
       we need to include it in the list of droppables so that the
       handling of the layers works
    */
    /*     if ( dataflowID === "-dataflow" ) {  */
    //true_dropdiv = dataflowDiv.parent().parent().parent().parent() ;
    /* This size of this div does not match the size of the drop
       area that the user sees
    */
    true_dropdiv = dataflowDiv.parent() ;
    true_dropdiv.data('corresponding_openmdao_object',this);
    openmdao.drag_and_drop_manager.addDroppable(true_dropdiv);

    /* dataflowDiv.droppable ({ */
    true_dropdiv.droppable ({
        accept: '.IComponent',

        out: function(ev,ui){
            // Does not look like I need this any more
            //var o = true_dropdiv.data('corresponding_openmdao_object');
            //o.unhighlightAsDropTarget() ;
            openmdao.drag_and_drop_manager.draggableOut( true_dropdiv ) ;
            // debug.info( "out of dataflow pane true drop div" ) ;

            /* Just used for debugging */
            //calculated_zindex = openmdao.drag_and_drop_manager.computeCalculatedZindex( true_dropdiv ) ;
            //topmost_zindex = openmdao.drag_and_drop_manager.computeTopmostZindex( true_dropdiv ) ;
            //debug.info ("out", elm.find(".DataflowFigureHeader")[0].innerHTML, calculated_zindex, topmost_zindex )
        },
        over: function(ev,ui){
            openmdao.drag_and_drop_manager.draggableOver( true_dropdiv ) ;
            //debug.info( "over dataflow pane true drop div" ) ;
            /* calculated_zindex = openmdao.drag_and_drop_manager.computeCalculatedZindex( elm ) ;
               topmost_zindex = openmdao.drag_and_drop_manager.computeTopmostZindex( elm ) ;
               debug.info ("over", elm.find(".DataflowFigureHeader")[0].innerHTML, calculated_zindex, topmost_zindex )
            */
        },
        drop: function(ev,ui) {
            /* divs could be in front of divs and the div that gets the drop
               event might not be the one that is in front visibly and therefore
               is not the div the user wants the drop to occur on
            */
            //top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
            top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
            if (top_div) {
                /* call the method on the correct div to handle the drop */
                var drop_function = top_div.droppable('option','actualDropHandler');
                drop_function(ev,ui);
            }
        },
        actualDropHandler: function(ev,ui) {
            var droppedObject = jQuery(ui.draggable).clone(),
            droppedName = droppedObject.text(),
            droppedPath = droppedObject.attr("modpath"),
            model = true_dropdiv.data("corresponding_openmdao_object").openmdao_model;

            openmdao.drag_and_drop_manager.clearHighlightingDroppables() ;
            openmdao.drag_and_drop_manager.clearDroppables() ;

            openmdao.Util.promptForValue('Enter name for new '+ droppedName,
                                         function(name) {
                                             model.addComponent(droppedPath,name,self.pathname);
                                         }
                                        );
        }
    });

    /** Highlight this pane when it the cursor is over it and it can accept a drop */
    this.highlightAsDropTarget=function() {
        dataflow.setBackgroundImage( "/static/images/grid_10_highlighted.png", true);
        //debug.info ("highlight dataflowpane", dataflowDiv.id);
    };

    /** Turn off highlighting of this pane when it can no
        longer accept a drop because the cursor is not over it
        or another drop target is over it */
    this.unhighlightAsDropTarget=function() {
        dataflow.setBackgroundImage( "/static/images/grid_10.png", true);
        //debug.info ("unhighlight dataflowpane", dataflowDiv.id);
    };


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

    this.showDataflow(pathname);
};
