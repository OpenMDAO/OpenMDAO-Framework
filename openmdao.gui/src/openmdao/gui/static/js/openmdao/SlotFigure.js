
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(model,pathname,containertype,klass,desc,filled) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var slotDiv = '<div height="50" width="100" style="margin:10px;clear:both;" />',
        slotSVG = '<svg height="50" width="100">'
                + '    <rect height="50" width="100" rx="15" ry="15" style="stroke-width:2; fill:white" />'
                + '    <text id="name" x="50" y="20" text-anchor="middle">Name</text>'
                + '    <text id="klass" x="50" y="40" font-style="italic" text-anchor="middle">Klass</text>'
                + '</svg>',
        fig = jQuery(slotDiv)
            .append(slotSVG),
        color = filled ? 'green' : 'red',
        name = openmdao.Util.getName(pathname);

    fig.find('#name').text(name);
    fig.find('#klass').text(klass);

    if ((containertype === 'list') && filled) {
        // add a rect for each filled list entry
        var i = filled;
        while(i > 0) {
            fig.append(slotSVG);
            i = i - 1;
        }
    }

    // set id and tooltip
    fig.attr('id','SlotFigure-'+(pathname.replace('.','-')));
    fig.attr('title',desc);

    // set up as drop target
    openmdao.drag_and_drop_manager.addDroppable(fig);
    fig.addClass("SlotFigure");
    fig.data('corresponding_openmdao_object',fig);
    fig.droppable ({
        accept: '.'+klass,
        out: function(ev,ui){
            debug.info('SlotFigure out',pathname);
            fig.unhighlightAsDropTarget() ;
            openmdao.drag_and_drop_manager.draggableOut(fig);
        },
        over: function(ev,ui){
            debug.info('SlotFigure over',pathname);
            openmdao.drag_and_drop_manager.draggableOver(fig);
        },
        drop: function(ev,ui) {
            debug.info('SlotFigure drop',pathname);
            var top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev, ui),
                drop_function = top_div.droppable( 'option', 'actualDropHandler');
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

    /** Highlight figure when cursor is over it and it can accept a drop */
    fig.highlightAsDropTarget=function(){
        fig.find('rect').css({'fill': 'rgb(207, 214, 254)'});
    };

    /** Unhighlight figure when it can no longer accept a drop */
    fig.unhighlightAsDropTarget=function(){
        fig.find('rect').css({'fill': 'white'});
    };

    /***********************************************************************
     *  protected
     ***********************************************************************/

    this.setState = function(klass, filled) {
        var r = fig.find('rect'),
            n = fig.find('#name'),
            k = fig.find('#klass');

        debug.info('SlotFigure.setState()',filled,pathname,r,r.length,n,k);

        // set colors & klass
        if (filled) {
            r.css({'stroke-dasharray':'none', 'stroke':color});
        }
        else {
            r.css({'stroke-dasharray':3, 'stroke':color});
        }
        n.css({'fill': color}).text(name);
        k.css({'fill': color}).text(klass);

        // for list and dict, there is one additional unfilled slot entry
        if (filled && r.length > 0) {
            r.filter(':last').css({'stroke-dasharray':3, 'stroke':'red'});
            n.filter(':last').css({'fill': 'red'});
            k.filter(':last').css({'fill': 'red'});
        }
    };

    // set initial state & return it
    this.setState(klass, filled);
    return fig;
};


