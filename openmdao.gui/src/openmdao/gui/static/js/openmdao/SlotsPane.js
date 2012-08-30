
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotsPane = function(elm,model,pathname,name,editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        slotsID = pathname.replace(/\./g,'-')+"-slots",
        slotsDiv = jQuery('<div style="position:relative; background-color:black;">')
            .appendTo(elm);

    self.pathname = pathname;

    elm.css({'overflow':'auto'});

    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            if (figures[slot.name]) {
                // update existing slot figure
                figures[slot.name].setState(slot.filled);
            }
            else {
                // create a new slot figure
                var fig = openmdao.SlotFigure(model, pathname+'.'+slot.name, slot),
                    figMenu = fig.getContextMenu();
                figures[slot.name] = fig;
                slotsDiv.append(fig);
                ContextMenu.set(figMenu.attr('id'), fig.attr('id'));
            }
        });
    }

    // all this is just to prevent drops from falling thru to underlying panes
    var true_dropdiv = slotsDiv.parent();
    true_dropdiv.data('corresponding_openmdao_object',this);
    true_dropdiv.droppable ({
        accept: '.objtype',
        out: function(ev,ui) {
            var o = true_dropdiv.data('corresponding_openmdao_object');
            openmdao.drag_and_drop_manager.draggableOut(true_dropdiv);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(true_dropdiv);
        },
        drop: function(ev,ui) {
            /* divs could be in front of divs and the div that gets the drop
               event might not be the one that is in front visibly and therefore
               is not the div the user wants the drop to occur on */
            var o = elm.data('corresponding_openmdao_object');
            top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
            /* call the method on the correct div to handle the drop */
            if (top_div) {
                var drop_function = top_div.droppable('option','actualDropHandler');
                drop_function(ev,ui);
            }
        }
    });

    this.highlightAsDropTarget=function() {
        // do nothing, but needed for DragAndDropManager
    };

    this.unhighlightAsDropTarget=function() {
        // do nothing, but needed for DragAndDropManager
    };

    /***********************************************************************
     *  protected
     ***********************************************************************/

    /** update slots diagram */
    this.loadData = function(json) {
        slotsDiv.html('');
        figures = {};
        if (Object.keys(json).length > 0) {
            updateFigures(json);
        }
    };
};
