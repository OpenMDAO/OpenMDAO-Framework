
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DragAndDropManager=function() {

    /***********************************************************************
     * Keep a hash table of all drop targets that are currently under a
     * dragged object.
     *
     * Drop targets (droppables) are added via the 'draggableOver'
     * method and removed with the 'draggableOut' method.
     *
     * The 'draggableOver' method will also call the 'updateHighlighting'
     * function, which will highlight the drop target with the highest
     * z-index and set it to be the current drop target. (It will also
     * remove the highlighting from all other drop targets)
     ***********************************************************************/

    var self = this,
        droppables= new Hashtable();

    self.drop_target = null;

    // get the current drop target, which is the highlighted target
    this.getTopDroppableForDropEvent = function(ev, ui) {
        return self.drop_target;
    };

    // gets called when the cursor goes outside the passed in droppable element
    // the input should be a jQuery object
    this.draggableOut = function(droppable) {
        droppables.remove(droppable[0].id);
        openmdao.drag_and_drop_manager.updateHighlighting();
    };

    // gets called when the cursor goes inside the passed in parameter element, droppable
    // the input should be a jQuery object
    this.draggableOver = function(droppable) {
        var elm_calculated_zindex = this.computeCalculatedZindex(droppable);
        droppables.put(droppable[0].id, elm_calculated_zindex);
        openmdao.drag_and_drop_manager.updateHighlighting();
    };

    // clear all droppables
    this.clearHighlightingDroppables = function() {
        droppables.each(function(id, zindex) {
            var div = $(id),
                div_object = jQuery(div),
                o = div_object.data('corresponding_openmdao_object');
            o.unhighlightAsDropTarget();
        });
        droppables.clear();
    };


    // Given the list of droppables that could potentially be
    //    dropped on given where the cursor is, figure out which is on top
    //    and tell it to highlight itself. Unhighlight all the others
    this.updateHighlighting = function() {
        // Find the div with the max id
        var max_zindex = -10000,
            max_topmost_zindex = -10000,
            max_id = "";

        //debug.info( "Starting calc of front div" ) ;
        droppables.each(function(id, zindex) {
            var div = $(id),
                div_object = jQuery(div),
                tmp_elm = div_object,
                calculated_zindex,
                count = 0,
                max_count = 0;

            while (tmp_elm.css("z-index") === "auto" && ! tmp_elm.is("body")) {
               tmp_elm = tmp_elm.parent();
            }
            calculated_zindex = tmp_elm.css("z-index");

            // Find the zindex for the topmost element
            tmp_elm = div_object;
            var topmost_zindex = null;
            while (! tmp_elm.is("body")) {
                if (tmp_elm.css("z-index") !== "auto") {
                    topmost_zindex = tmp_elm.css("z-index");
                }
                if (! tmp_elm.parent() || tmp_elm.parent().is("body")) {
                    break;
                }
                tmp_elm = tmp_elm.parent();
                count += 1;
            }
            topmost_zindex = tmp_elm.css("z-index");

            if (topmost_zindex > max_topmost_zindex) {
                max_id = id;
                max_zindex = calculated_zindex;
                max_topmost_zindex = topmost_zindex;
                max_count = count;
            }
            else if ( topmost_zindex === max_topmost_zindex ) {
                /* Use the count to break the tie */
                if (count > max_count) {
                    max_zindex = calculated_zindex;
                    max_id = id;
                    max_count = count;
                }
                else if (count === max_count) {
                    /* If still tied, use the zindex to break the tie */
                    if (calculated_zindex > max_zindex) {
                        max_id = id;
                        max_zindex = calculated_zindex;
                    }
                }
            }
        });

        // Now only highlight the top one
        droppables.each( function(id, zindex) {
            var div = $(id),
                div_object = jQuery(div),
                o = div_object.data('corresponding_openmdao_object');
            if (id === max_id) {
                /* We only allow dropping onto Assemblies and a good way to check that
                  is the maxmin variable. We also allow dropping onto the top, which is the  */
                if ((id === "dataflow_pane" ) ||
                    (div_object.attr("class").substring(0,14) === "DataflowFigure") ||
                    (div_object.attr("class").indexOf("SlotFigure") !== -1)) {
                    o.highlightAsDropTarget();
                    self.drop_target = div_object;
                }
                else {
                    self.drop_target = null;
                }
            }
            else {
                o.unhighlightAsDropTarget();
            }
        });

    };

    // Find the zindex for the jQuery object by finding the first parent that
    //    has a non-auto zindex
    this.computeCalculatedZindex = function(elm) {
        var tmp_elm = elm;
        while (tmp_elm.css("z-index") === "auto"  && ! tmp_elm.is("body")) {
            tmp_elm = tmp_elm.parent();
        }
        calculated_zindex = tmp_elm.css("z-index");
        return calculated_zindex ;
    };

    // Find the zindex for the topmost element, the input is a jQuery object
    this.computeTopmostZindex = function(elm) {
        var tmp_elm = elm,
            topmost_zindex = null ;
        while (tmp_elm.parent() && ! tmp_elm.is("body")) {
            tmp_elm = tmp_elm.parent();
            if (tmp_elm.css( "z-index" ) !== "auto") {
                topmost_zindex = tmp_elm.css("z-index");
            }
        }
        topmost_zindex = tmp_elm.css("z-index");
        return topmost_zindex;
    };

    /**********************************************************************
       For handling drag and drop on workflows
    **********************************************************************/
    var workflow_droppables= new Hashtable(),
        drop_workflow_target = null ;

    // get workflow figure top div that contains the draggable, is visible and
    // accepts the type of the draggable
    this.getTopWorkflowDroppableForDropEvent = function(ev, ui) {
        return openmdao.drag_and_drop_manager.drop_workflow_target;
    };

    // the input should be a jQuery object
    this.draggableWorkflowOut = function(droppable, dropped_pathname) {
        workflow_droppables.remove(droppable[0].id);
        openmdao.drag_and_drop_manager.updateWorkflowHighlighting(dropped_pathname);
    };

    // gets called when the cursor goes inside the passed in droppable element
    // the input should be a jQuery object
    this.draggableWorkflowOver = function(droppable, dropped_pathname) {
        var elm_calculated_zindex = this.computeCalculatedZindex(droppable);
        workflow_droppables.put(droppable[0].id, elm_calculated_zindex);
        openmdao.drag_and_drop_manager.updateWorkflowHighlighting(dropped_pathname);
    };

    // Given the list of droppables that could potentially be
    //    dropped on given where the cursor is, figure out which is on top
    //    and tell it to highlight itself. Unhighlight all the others
    this.updateWorkflowHighlighting = function(dropped_pathname) {
        // Find the div with the max id
        var max_zindex = -10000,
            max_topmost_zindex = -10000,
            max_id = "";

        workflow_droppables.each(function(id, zindex) {
            var div = $(id),
                div_object = jQuery(div),
                tmp_elm = div_object,
                calculated_zindex,
                count = 0,
                max_count = 0,
                topmost_zindex;

            while (tmp_elm.css("z-index") === "auto") {
                tmp_elm = tmp_elm.parent();
            }
            calculated_zindex = tmp_elm.css("z-index");

            // Find the zindex for the topmost element
            tmp_elm = div_object;
            while (! tmp_elm.is("body")) {
                if (tmp_elm.css( "z-index" ) !== "auto") {
                    topmost_zindex = tmp_elm.css("z-index");
                }
                if (! tmp_elm.parent() || tmp_elm.parent().is("body")) {
                    break;
                }
                tmp_elm = tmp_elm.parent();
                count += 1;
            }
            topmost_zindex = tmp_elm.css("z-index");

            if (topmost_zindex > max_topmost_zindex) {
                max_id = id;
                max_zindex = calculated_zindex;
                max_topmost_zindex = topmost_zindex;
                max_count = count;
            }
            else if (topmost_zindex === max_topmost_zindex) {
                /* Use the count to break the tie */
                if (count > max_count) {
                    max_zindex = calculated_zindex;
                    max_id = id;
                    max_count = count;
                }
                else if (count === max_count) {
                    /* If still tied, use the zindex to break the tie */
                    if (calculated_zindex > max_zindex) {
                        max_id = id;
                        max_zindex = calculated_zindex;
                    }
                }
            }
        });

       // Now only highlight the top one
        workflow_droppables.each(function(id, zindex) {
            var div = $(id),
                div_object = jQuery(div),
                o = div_object.data('corresponding_openmdao_object');

            if (id === max_id) {
                o.highlightAsDropTarget();
                openmdao.drag_and_drop_manager.drop_workflow_target = div_object;
            }
            else {
                o.unhighlightAsDropTarget();
            }
        });
    };

    // clear all droppables
    this.clearHighlightingWorkflowDroppables = function() {
        workflow_droppables.each(function(id, zindex) {
            var div = $(id),
                div_object = jQuery(div),
                o = div_object.data('corresponding_openmdao_object');
            o.unhighlightAsDropTarget();
        });
        workflow_droppables.clear();
    };
};
