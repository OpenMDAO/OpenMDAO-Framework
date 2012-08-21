
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(model,pathname,klass,filled){
    var slotHTML = '<div height="50" width="100" style="margin:10px;float:left;">'
                 + '<svg height="50" width="100">'
                 + '    <rect height="50" width="100" rx="15" ry="15" style="stroke-width:2; fill:white" />'
                 + '    <text id="name" x="50" y="20" text-anchor="middle">Name</text>'
                 + '    <text id="klass" x="50" y="40" font-style="italic" text-anchor="middle">Klass</text>'
                 + '</svg>'
                 + '</div>',
        fig = jQuery(slotHTML),
        color = filled ? 'green' : 'red',
        name = openmdao.Util.getName(pathname);

    // set colors
    if (filled) {
        fig.find('rect').css({'stroke-dasharray':'none', 'stroke':color});
    }
    else {
        fig.find('rect').css({'stroke-dasharray':3, 'stroke':color});
    }
    fig.find('#name').css({'fill': color}).text(name);
    fig.find('#klass').css({'fill': color}).text(klass);

    // set id and tooltip
    fig.attr('id','SlotFigure-'+pathname);
    fig.attr('title',name);

    // store refs in object
    fig.model = model;
    fig.pathname = name;
    fig.klass = klass;
    fig.filled = filled;

    // set up as drop target
    openmdao.drag_and_drop_manager.addDroppable(fig);
    fig.addClass("SlotFigure");
    fig.data('corresponding_openmdao_object',fig);
    fig.droppable ({
        accept: '.'+klass,
        out: function(ev,ui){
            fig.unhighlightAsDropTarget() ;
            openmdao.drag_and_drop_manager.draggableOut(fig);
        },
        over: function(ev,ui){
            openmdao.drag_and_drop_manager.draggableOver(fig);
        },
        drop: function(ev,ui) {
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
        fig.find('rect').css({'fill': 'gray'});
    };

    /** Unhighlight figure when it can no longer accept a drop */
    fig.unhighlightAsDropTarget=function(){
        fig.find('rect').css({'fill': 'white'});
    };

    return fig;
};


