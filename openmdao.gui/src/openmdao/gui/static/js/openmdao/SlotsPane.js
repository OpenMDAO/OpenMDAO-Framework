
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotsPane = function(elm,model,pathname,name,editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        slotsID = "#"+pathname.replace(/\./g,'-')+"-slots",
        slotsCSS = 'height:'+(screen.height-100)+'px;'+
                   'width:'+(screen.width-100)+'px;'+
                   'position:relative; background-color:black;',
        slotsDiv = jQuery('<div id='+slotsID+' style="'+slotsCSS+'">').appendTo(elm),
        slots = new draw2d.Workflow(slotsID),
        true_dropdiv = slotsDiv.parent();

    self.pathname = pathname;

    slots.setBackgroundImage( null, true);
    elm.css({ 'overflow':'auto' });
    slots.setViewPort(elm.attr('id'));

    true_dropdiv.data('corresponding_openmdao_object',this);
    openmdao.drag_and_drop_manager.addDroppable(true_dropdiv);

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

    // TODO: Do I really need these ?
    this.highlightAsDropTarget=function() {
        debug.info ("highlight slotspane", slotsDiv[0].id ) ;
    };

    this.unhighlightAsDropTarget=function() {
        debug.info ("unhighlight slotspane", slotsDiv[0].id ) ;
    };

    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            var name = slot.name,
                type = slot.klass,
                filled = slot.filled,
                fig;

            if (self.pathname) {
                fig = new openmdao.SlotFigure(model,self.pathname+'.'+name,type,filled);
            }
            else {
                fig = new openmdao.SlotFigure(model,name,type,filled);
            }

            fig.setTitle(name);
            figures[name] = fig;
            fig.setContent('<center>(('+type+'))'+'</center>');
            // TODO: flexible grid layout (adjusting for size)
            var count = Object.keys(figures).length,
                x = (count-1)*(fig.getWidth()+20)  + 20,
                y = 20;
            slots.addFigure(fig,x,y);
        });
    }

    /***********************************************************************
     *  protected
     ***********************************************************************/

    /** update slots diagram */
    this.loadData = function(json) {
        slots.clear();
        figures = {};
        if (Object.keys(json).length > 0) {
            updateFigures(json,false);
        }
    };

};
