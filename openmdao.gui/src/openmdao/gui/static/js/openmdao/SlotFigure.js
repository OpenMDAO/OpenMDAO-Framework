
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(model,pathname,slot) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        id = 'SlotFigure-'+pathname.replace(/\./g,'-'),
        slotDiv = '<div class="SlotFigure" style="margin:10px;clear:both;" />',
        slotSVG = '<svg height="50" width="100">'
                + '    <rect height="50" width="100" rx="15" ry="15" style="stroke-width:2; fill:white" />'
                + '    <text id="name" x="50" y="20" text-anchor="middle">Name</text>'
                + '    <text id="klass" x="50" y="40" font-style="italic" text-anchor="middle">Klass</text>'
                + '</svg>',
        fig = jQuery(slotDiv)
            .append(slotSVG),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(fig);

    // set name, id, tooltip and width
    fig.find('#name').text(slot.name);
    fig.attr('id',id);
    fig.attr('title',slot.desc);
    fig.width(100);

    // create context menu
    contextMenu.append(jQuery('<li>Remove Contents</li>').click(function(e) {
        if (fig.hasClass('filled')) {
            var figOffset, idx, cmd;
            if (slot.containertype === 'list') {
                figOffset = fig.offset();
                idx = Math.floor((e.pageX - figOffset.left) / 100);
                cmd = 'del '+pathname+'['+ idx+']';
            }
            else {
                 cmd = pathname+' =  None';
            }
            model.issueCommand(cmd);
        }
        else {
            openmdao.Util.notify('Slot is already empty!');
        }
    }));

    /** provide access to fig's context menu (for use after fig is in the DOM */
    fig.getContextMenu = function() {
        return contextMenu;
    };

    /** open object editor on double click */
    fig.dblclick(function(e) {
        if (fig.hasClass('filled')) {
            if (slot.containertype === 'singleton') {
                new openmdao.ObjectFrame(model, pathname);
            }
            else {
                var figOffset = fig.offset();
                    idx = Math.floor((e.pageX - figOffset.left) / 100);
                new openmdao.ObjectFrame(model, pathname+'['+idx+']');
            }
        }
    });

    /** Highlight figure when cursor is over it and it can accept a drop */
    fig.highlightAsDropTarget=function() {
        fig.find('rect').filter(':last').css({'fill': '#CFD6FE'});
    };

    /** Unhighlight figure when it can no longer accept a drop */
    fig.unhighlightAsDropTarget=function() {
        fig.find('rect').filter(':last').css({'fill': 'white'});
    };

    // set up as drop target
    fig.data('corresponding_openmdao_object',fig);
    fig.droppable ({
        accept: '.'+slot.klass,
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
                slotParent = openmdao.Util.getPath(pathname),
                cmd = (slot.containertype === 'list' ?
                    pathname + '.append(create("'+droppedPath+'"))' :
                    slotParent + '.add("'+slot.name+'", create("'+droppedPath+'"))');
            model.issueCommand(cmd);
            openmdao.drag_and_drop_manager.clearHighlightingDroppables();
        }
    });

    /***********************************************************************
     *  protected
     ***********************************************************************/

    this.setState = function(value) {
        var r = fig.find('rect'),
            n = fig.find('#name'),
            k = fig.find('#klass'),
            filled = (slot.containertype === 'singleton' && value !== null) ||
                     (slot.containertype === 'list' && value.length > 0),
            color = filled ? 'green' : 'red';

        if (filled) {
            fig.addClass('filled');  // TODO: set colors etc via CSS?
            if (slot.containertype === 'singleton') {
                r.css({'stroke-dasharray':'none', 'stroke':color});
                n.css({'fill': color});
                k.css({'fill': color}).text(value);
            }
            else if (slot.containertype === 'list') {
                // rebuild figure with a rect for each filled list entry
                fig.find('svg').remove();
                var i = 0;
                while(i < value.length) {
                    fig.append(slotSVG);
                    fig.find('#name').filter(':last').text(slot.name+'['+i+']');
                    fig.find('#klass').filter(':last').text(value[i]);
                    i = i + 1;
                }
                fig.append(slotSVG);  // add empty spot for adding to list

                // set all to filled
                r = fig.find('rect').css({'stroke-dasharray':'none', 'stroke':color});
                n = fig.find('#name').css({'fill': color});
                k = fig.find('#klass').css({'fill': color});

                // set last to unfilled
                r.filter(':last').css({'stroke-dasharray':3, 'stroke':'red'});
                n.filter(':last').css({'fill': 'red'}).text(slot.name);
                k.filter(':last').css({'fill': 'red'}).text(slot.klass+'[]');

                fig.width(100*(value.length+1));
            }
            else {
                debug.warm('SlotFigure - Unrecognized slot type:',slot.containertype);
            }
        }
        else {
            fig.removeClass('filled');
            r.css({'stroke-dasharray':3, 'stroke':color});
            n.css({'fill': color}).text(slot.name);
            if (slot.containertype === 'list') {
                k.css({'fill': color}).text(slot.klass+'[]');
            }
            else {
                k.css({'fill': color}).text(slot.klass);
            }
        }
    };

    // set initial state & return it
    this.setState(slot.filled);
    return fig;
};


