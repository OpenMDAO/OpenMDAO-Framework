
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(model,pathname,slot) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        id = 'SlotFigure-'+pathname.replace(/\./g,'-'),
        slotDiv = '<div class="SlotFigure" style="margin:10px; clear:both;" />',
        slotSVG = '<svg height="60" width="100">'
                + '    <rect x="0" y="5" height="50" width="100" rx="15" ry="15";" />'
                + '    <text id="name" x="50" y="25" text-anchor="middle">Name</text>'
                + '    <text id="klass" x="50" y="45" font-style="italic" text-anchor="middle">Klass</text>'
                + '</svg>',
        lbrkSVG = '<svg height="60" width="20">'
                + '    <text x="0" y="45" font-size="60" style="fill:gray">[</text>'
                + '</svg>',
        commSVG = '<svg height="60" width="20">'
                + '    <text x="0" y="45" font-size="60" style="fill:gray">,</text>'
                + '</svg>',
        rbrkSVG = '<svg height="60" width="20">'
                + '    <text x="0" y="45" font-size="60" style="fill:gray">]</text>'
                + '</svg>',
        fig = slot.containertype === 'singleton' ?
                jQuery(slotDiv).append(slotSVG) :
                jQuery(slotDiv).append(lbrkSVG).append(slotSVG).append(commSVG).append(rbrkSVG),
        filledRectCSS = {'stroke-width':4, 'stroke-dasharray':'none', 'stroke':'#0b93d5', 'fill': 'gray'},
        filledTextCSS = {'fill': 'black'},
        unfilledRectCSS = {'stroke-width':2, 'stroke-dasharray':8, 'stroke':'gray', 'fill': 'none'},
        unfilledTextCSS = {'fill': 'gray'},
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(fig);

    // set name, id, tooltip and width
    fig.find('#name').text(slot.name);
    fig.attr('id',id);
    fig.attr('title',slot.desc);

    // create context menu
    contextMenu.append(jQuery('<li>Remove Contents</li>').click(function(e) {
        if (fig.hasClass('filled')) {
            var figOffset, idx, cmd;
            if (slot.containertype === 'list') {
                figOffset = fig.offset();
                idx = Math.floor((e.pageX - figOffset.left) / 120);
                cmd = 'del '+pathname+'['+ idx+']';
            }
            else {
                cmd = openmdao.Util.getPath(pathname)+".remove('"+openmdao.Util.getName(pathname)+"')";
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
        if (fig.hasClass('filled') && slot.containertype === 'singleton') {
            fig.find('rect').filter(':last').css(filledRectCSS);
        }
        else {
            fig.find('rect').filter(':last').css(unfilledRectCSS);
        }
    };

    // set up as drop target
    fig.droppable ({
        accept: '.'+slot.klass,
        greedy: true,
        out: function(ev,ui) {
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
            // could get same event multiple times if drop triggers for sibling targets
            if (this.dropEvent && this.dropEvent === ev.originalEvent) {
                return;  // already handled this drop event
            }
            this.dropEvent = ev.originalEvent;

            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("modpath");

            openmdao.drag_and_drop_manager.clearHighlightingDroppables();

            openmdao.model.getSignature(droppedPath, function(signature) {
                if (signature.args.length) {
                    var prompt = 'Enter arguments for new '+droppedName;
                    openmdao.Util.promptForArgs(prompt, signature, function(nm, args) {
                        var cmd = 'create("'+droppedPath+'"'+args+')';
                        if (slot.containertype === 'list') {
                            cmd = pathname+'.append('+cmd+')';
                        }
                        else {
                            var slotParent = openmdao.Util.getPath(pathname);
                            if (slot.containertype === 'singleton' && slot.filled !== null) {
                                cmd = slotParent+'.replace("'+slot.name+'", '+cmd+')';
                            }
                            else {
                                cmd = slotParent+'.add("'+slot.name+'", '+cmd+')';
                            }
                        }
                        model.issueCommand(cmd);
                    }, true);
                }
                else {
                    var cmd = 'create("'+droppedPath+'")';
                    if (slot.containertype === 'list') {
                        cmd = pathname+'.append('+cmd+')';
                    }
                    else {
                        var slotParent = openmdao.Util.getPath(pathname);
                        if (slot.containertype === 'singleton' && slot.filled !== null) {
                            cmd = slotParent+'.replace("'+slot.name+'", '+cmd+')';
                        }
                        else {
                            cmd = slotParent+'.add("'+slot.name+'", '+cmd+')';
                        }
                    }
                    model.issueCommand(cmd);
                }
            });
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
                     (slot.containertype === 'list' && value.length > 0);

        if (filled) {
            fig.addClass('filled');
            if (slot.containertype === 'singleton') {
                r.css(filledRectCSS);
                n.css(filledTextCSS);
                k.css(filledTextCSS).text(value);
                fig.height(60);
                fig.width(100);
            }
            else if (slot.containertype === 'list') {
                // rebuild figure with a rect for each filled list entry
                fig.find('svg').remove();
                fig.append(lbrkSVG);

                var i = 0;
                while(i < value.length) {
                    fig.append(slotSVG);
                    fig.find('#name').filter(':last').text(slot.name+'['+i+']');
                    fig.find('#klass').filter(':last').text(value[i]);
                    i = i + 1;
                    fig.append(commSVG);
                }

                fig.append(slotSVG);  // add empty spot for adding to list
                fig.append(commSVG);
                fig.append(rbrkSVG);

                // set all to filled
                r = fig.find('rect').css(filledRectCSS);
                n = fig.find('#name').css(filledTextCSS);
                k = fig.find('#klass').css(filledTextCSS);

                // set last to unfilled
                r.filter(':last').css(unfilledRectCSS);
                n.filter(':last').css(unfilledTextCSS).text(slot.name);
                k.filter(':last').css(unfilledTextCSS).text(slot.klass);

                fig.height(60);
                fig.width(120*(value.length+1)+40);
            }
            else {
                debug.warm('SlotFigure - Unrecognized slot type:',slot.containertype);
            }
        }
        else {
            fig.removeClass('filled');
            r.css(unfilledRectCSS);
            n.css(unfilledTextCSS).text(slot.name);
            if (slot.containertype === 'list') {
                k.css(unfilledTextCSS).text(slot.klass);
                fig.height(60);
                fig.width(160);
            }
            else {
                k.css(unfilledTextCSS).text(slot.klass);
                fig.height(60);
                fig.width(100);
            }
        }
    };

    // set initial state & return it
    this.setState(slot.filled);
    return fig;
};
