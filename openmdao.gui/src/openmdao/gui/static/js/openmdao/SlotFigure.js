/***********************************************************************
 * SlotFigure: A graphical representation of a Slot.
 *
 * A singleton slot is rendered as a dash-outlined component-like figure.
 * Dropping an object type on a figure will fill the slot with an object
 * of that type. A list slot is represented as a comma saparated list of
 * singleton slot figures in square brackets. Dropping an object type on
 * a list slot figure will add an object of that type to the list slot.
 *
 *  Arguments:
 *      elm:      the parent element in the DOM for this figure
 *      model:    object that provides access to the openmdao model
 *      pathname: the pathname of the slot
 *      slot:     the contents of the slot
 *      inDict:   true if this slot is in a dictionary
 ***********************************************************************/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(elm, model, pathname, slot, inDict) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        id = 'SlotFigure-' + pathname.replace(/\.|\[|\]|\"|\'/g, '-'),
        slotDiv = '<div class="SlotFigure">',
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
        singletonCSS = {'margin': '10px', 'clear': 'both'},
        dictionaryCSS = {'margin': '10px', 'display': 'inline-block'},
        filledRectCSS = {'stroke-width': 4, 'stroke-dasharray': 'none', 'stroke': '#0b93d5', 'fill': 'gray'},
        filledTextCSS = {'fill': 'black'},
        unfilledRectCSS = {'stroke-width': 2, 'stroke-dasharray': 8, 'stroke': 'gray', 'fill': 'none'},
        unfilledTextCSS = {'fill': 'gray'},
        fig = jQuery(slotDiv)
            .appendTo(elm),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(fig);

    if (slot.containertype === 'singleton') {
        if (inDict) {
            fig.css(dictionaryCSS);
        }
        else {
            fig.css(singletonCSS);
        }
        fig.append(slotSVG);
    }
    else if (slot.containertype === 'list') {
        fig.css(singletonCSS);
        fig.append(lbrkSVG).append(slotSVG).append(commSVG).append(rbrkSVG);
    }

    // set name, id, and title
    fig.find('#name').text(slot.name);
    fig.attr('id',id);
    fig.attr('title',slot.desc);

    // create context menu
    contextMenu.append(jQuery('<li title="Edit object">Edit</li>').click(function(e) {
        if (fig.hasClass('filled')) {
            var figOffset,
                idx,
                editor;
            if (slot.containertype === 'list') {
                figOffset = fig.offset();
                idx = Math.floor((e.pageX - figOffset.left) / 120);
                editor = new openmdao.ObjectFrame(model, pathname+'['+ idx+']');
            }
            else {
                editor = new openmdao.ObjectFrame(model, pathname);
            }
        }
        else {
            openmdao.Util.notify('Slot is empty!');
        }
    }));
    contextMenu.append(jQuery('<li title="Remove object from slot">Remove</li>').click(function(e) {
        if (fig.hasClass('filled')) {
            var figOffset,
                idx,
                cmd;
            if (slot.containertype === 'list') {
                figOffset = fig.offset();
                idx = Math.floor((e.pageX - figOffset.left) / 120);
                cmd = 'del '+pathname+'['+ idx+']';
            }
            else {
                if (inDict) {
                    cmd = pathname + " = None";
                }
                else {
                    cmd = openmdao.Util.getPath(pathname)
                        + ".remove('"+openmdao.Util.getName(pathname)+"')";
                }
            }
            model.issueCommand(cmd);
        }
        else {
            openmdao.Util.notify('Slot is already empty!');
        }
    }));
    ContextMenu.set(contextMenu.attr('id'), fig.attr('id'));

    /** open object editor on double click */
    fig.dblclick(function(e) {
        if (fig.hasClass('filled')) {
            if (slot.containertype === 'singleton') {
                new openmdao.ObjectFrame(model, pathname);
            }
            else if (slot.containertype === 'list') {
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
                                if (inDict) {
                                    cmd = pathname + '=' + cmd;
                                }
                                else {
                                    cmd = slotParent+'.add("'+slot.name+'", '+cmd+')';
                                }
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
                        var slotParent = openmdao.Util.getPath(pathname),
                            realSlotParent = inDict ? pathname.slice(0,-slot.name.length - 1 ) : undefined;
                        if (slot.containertype === 'singleton' && slot.filled !== null) {
                            if (inDict) {
                                cmd = pathname + '=' + cmd;
                            }
                            else {
                                cmd = slotParent+'.replace("'+slot.name+'", '+cmd+')';
                            }
                        }
                        else {
                            if (inDict) {
                                cmd = pathname + '=' + cmd;
                            }
                            else {
                                cmd = slotParent+'.add("'+slot.name+'", '+cmd+')';
                            }
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

    /* update the figure to represent the filled state of the slot */
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
                k.css(filledTextCSS).text(value.split(".").pop());
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
                debug.warn('SlotFigure - Unrecognized slot type:',slot.containertype);
            }
        }
        else { // if not filled
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

    /** set the size of the figure to be wide enough to contain it's text */
    this.resize = function() {
        var name_text = fig.find('#name')[0],
            klass_text = fig.find('#klass')[0],
            name_width = name_text.getBBox().width,
            klass_width = klass_text.getBBox().width,
            rect_width = Math.max(name_width, klass_width) || 100,
            svg = fig.find("svg")[0];
            rect = fig.find("rect")[0];

        svg.setAttribute("width", rect_width + 40 ) ;
        rect.setAttribute("width", rect_width + 40 ) ;
        name_text.setAttribute("x", (rect_width + 40) / 2) ;
        klass_text.setAttribute("x", (rect_width + 40) / 2) ;
        fig.width(rect_width + 45);
    };

    // set initial state & resize it
    this.setState(slot.filled);
    // this.resize();
};

/***********************************************************************
 * SlotDictFigure: A graphical representation of a dictionary of slots.
 *
 * A dictionary of slots is represented as a comma saparated list of
 * singleton slot figures in curly brackets.
 *
 *  Arguments:
 *      elm:      the parent element in the DOM for this figure
 *      model:    object that provides access to the openmdao model
 *      pathname: the pathname of the slot
 *      slot:     the contents of the slot dictionary
 ***********************************************************************/

openmdao.SlotDictFigure=function(elm, model, pathname, slot) {
    /***********************************************************************
     *  private
     ***********************************************************************/
    var lcrlySVG = '<svg height="60" width="20">'
                 + '    <text x="0" y="45" font-size="60" style="fill:gray">{</text>'
                 + '</svg>',
        commaSVG = '<svg height="60" width="20">'
                 + '    <text x="0" y="45" font-size="60" style="fill:gray">,</text>'
                 + '</svg>',
        rcrlySVG = '<svg height="60" width="20">'
                 + '    <text x="0" y="45" font-size="60" style="fill:gray">}</text>'
                 + '</svg>',
        dictDiv  = jQuery('<div style="margin:10px; clear:both;">'),
        slotCnt  = Object.keys(slot.filled).length;

    // don't bother if there are no slots in the dict (nothing to see)
    if (slotCnt > 0) {
        elm.append(dictDiv);

        dictDiv.append(lcrlySVG);

        jQuery.each(slot.filled, function(key, val) {
            var slot_info = {
                containertype: "singleton",
                desc: slot.desc + ": " + key,
                filled: val,
                klass: val ? val: slot.klass,
                name: key
            };

            openmdao.SlotFigure(dictDiv, model, pathname+'["'+key+'"]', slot_info, true);

            if (slotCnt > 1) {
                dictDiv.append(commaSVG);
            }
            slotCnt = slotCnt - 1;
        });

        dictDiv.append(rcrlySVG);
    }

};
