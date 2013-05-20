/***********************************************************************
 *  DragAndDropManager: Keeps a hash table of all drop targets that are
 *                      currently under a dragged object.
 *
 * Drop targets (droppables) are added via the 'draggableOver'
 * method and removed with the 'draggableOut' method.
 *
 * The 'draggableOver' method will also call the 'updateHighlighting'
 * function, which will highlight the drop target with the highest
 * z-index and set it to be the current drop target. It will also
 * remove the highlighting from all other drop targets. Note that a
 * droppable must implement both the "highlightAsDropTarget()" and
 * "unhighlightAsDropTarget()" functions to be a valid drop target.
 * The 'reset' method will clear all highlighting and set the drop
 * target back to null.
 ***********************************************************************/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DragAndDropManager=function() {

    var _self = this,
        _droppables= {},
        _drop_target = null;

    // get the current drop target, which is the highlighted target
    this.getDropTarget = function() {
        return _drop_target;
    };

    // remove the droppable with the given id from the valid drop targets
    this.draggableOut = function(droppable) {
        if (droppable.hasOwnProperty("unhighlightAsDropTarget")) {
            droppable.unhighlightAsDropTarget();
        }
        delete _droppables[droppable.attr('id')];
        openmdao.drag_and_drop_manager.updateHighlighting();
    };

    // add a valid drop target with the given id
    this.draggableOver = function(droppable) {
        _droppables[droppable.attr('id')] = droppable;
        openmdao.drag_and_drop_manager.updateHighlighting();
    };

    // clear all drop targets
    this.reset = function() {
        jQuery.each(_droppables, function(id, droppable) {
            if (droppable.hasOwnProperty("unhighlightAsDropTarget")) {
                droppable.unhighlightAsDropTarget();
            }
        });
        _droppables = {};
        _drop_target = null;
    };

    // find the valid drop target with the highest z-index, highlight it and
    // set it to be the current drop target. Unhighlight all the others.
    this.updateHighlighting = function() {
        // Find the div with the max id
        var max_zindex = -10000,
            max_topmost_zindex = -10000,
            max_id = "";

        jQuery.each(_droppables, function(id, droppable) {
            var div_object = jQuery('#'+id),
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

        _drop_target = null;

        // Now only highlight the top one & set it as the drop target
        jQuery.each(_droppables, function(id, droppable) {
            if (id === max_id) {
                if (droppable.hasOwnProperty("highlightAsDropTarget")) {
                    droppable.highlightAsDropTarget();
                    _drop_target = droppable;
                }
            }
            else if (droppable.hasOwnProperty("unhighlightAsDropTarget")) {
                droppable.unhighlightAsDropTarget();
            }
        });
    };

    // Find zindex of element by finding first parent that has a non-auto zindex
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

};
