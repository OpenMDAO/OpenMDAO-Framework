
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(model,pathname,containertype,klass,desc,value) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        slotDiv = '<div height="50" width="100" class="SlotFigure" style="margin:10px;clear:both;" />',
        slotSVG = '<svg height="50" width="100">'
                + '    <rect height="50" width="100" rx="15" ry="15" style="stroke-width:2; fill:white" />'
                + '    <text id="name" x="50" y="20" text-anchor="middle">Name</text>'
                + '    <text id="klass" x="50" y="40" font-style="italic" text-anchor="middle">Klass</text>'
                + '</svg>',
        fig = jQuery(slotDiv)
            .append(slotSVG),
        name = openmdao.Util.getName(pathname);

    // set name, id, tooltip and width
    fig.find('#name').text(name);
    fig.attr('id','SlotFigure-'+(pathname.replace('.','-')));
    fig.attr('title',desc);
    fig.width(100);

    // open object editor on double click
    fig.dblclick(function() {
        if (fig.hasClass('filled')) {
            var editor = new openmdao.ObjectFrame(model, pathname);
        }
    });

    /** Highlight figure when cursor is over it and it can accept a drop */
    fig.highlightAsDropTarget=function() {
        fig.css({'background-color': 'rgb(207, 214, 254)'});
        fig.find('rect').css({'fill': '#CFD6FE'});
    };

    /** Unhighlight figure when it can no longer accept a drop */
    fig.unhighlightAsDropTarget=function() {
        fig.css({'background-color': 'transparent'});
        fig.find('rect').css({'fill': 'white'});
    };

    // set up as drop target
    fig.data('corresponding_openmdao_object',fig);
    fig.droppable ({
        accept: '.'+klass,
        out: function(ev,ui) {
            fig.unhighlightAsDropTarget() ;
            openmdao.drag_and_drop_manager.draggableOut(fig);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(fig);
        },
        drop: function(ev,ui) {
            var top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev, ui),
                drop_function = top_div.droppable('option', 'actualDropHandler');
            drop_function(ev, ui);
        },
        actualDropHandler: function(ev,ui) {
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedPath = droppedObject.attr("modpath"),
                cmd = openmdao.Util.getPath(pathname)+'.add("'+name+'", create("'+droppedPath+'"))';
            model.issueCommand(cmd);
            openmdao.drag_and_drop_manager.clearHighlightingDroppables();
        }
    });

    /***********************************************************************
     *  protected
     ***********************************************************************/

    this.setValue = function(value) {
        var r = fig.find('rect'),
            n = fig.find('#name'),
            k = fig.find('#klass'),
            filled = (value !== null),
            color = filled ? 'green' : 'red';

        if (filled) {
            // set colors & klass (TODO: use CSS to do this automatically?)
            fig.addClass('filled');
            r.css({'stroke-dasharray':'none', 'stroke':color});

            if ((containertype === 'singleton')) {
                fig.find('#klass').text(value.type);
            }
            else if ((containertype === 'list')) {
                // rebuild figure with a rect for each filled list entry
                r.remove();
                var i = 0;
                while(i < value.length) {
                    fig.append(slotSVG);
                    fig.find('#klass').filter(':last').text(value[i].type);
                    i = i + 1;
                }
                r = fig.find('rect');
                n = fig.find('#name');
                k = fig.find('#klass');
                fig.width(100*value.length);
            }
        }
        else {
            fig.removeClass('filled');
            r.css({'stroke-dasharray':3, 'stroke':color});
        }
        n.css({'fill': color}).text(name);
        k.css({'fill': color}).text(klass);

        // for list and dict, there is one additional unfilled slot entry
        if (filled && r.length > 1) {
            r.filter(':last').css({'stroke-dasharray':3, 'stroke':'red'});
            n.filter(':last').css({'fill': 'red'});
            k.filter(':last').css({'fill': 'red'});
        }
    };

    // set initial state & return it
    this.setValue(value);
    return fig;
};


