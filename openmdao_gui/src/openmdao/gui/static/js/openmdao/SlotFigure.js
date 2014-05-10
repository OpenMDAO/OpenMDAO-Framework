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
 *      elm:       the parent element in the DOM for this figure
 *      project:   object that provides access to the openmdao project
 *      pathname:  the pathname of the slot
 *      slot:      the attributes/contents of the slot
 ***********************************************************************/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(elm, project, pathname, slot) {
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
        singletonCSS = {'margin': '5px', 'clear': 'both'},
        collectionCSS = {'margin': '5px', 'display': 'inline-block'},
        filledRectCSS = {'stroke-width': 4, 'stroke-dasharray': 'none', 'stroke': '#0b93d5', 'fill': 'gray'},
        filledTextCSS = {'fill': 'black'},
        unfilledRectCSS = {'stroke-width': 2, 'stroke-dasharray': 8, 'stroke': 'gray', 'fill': 'none'},
        unfilledTextCSS = {'fill': 'gray'},
        fig = jQuery(slotDiv)
            .appendTo(elm),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(fig);

    if (slot.containertype === 'singleton') {
        fig.css(singletonCSS);
    }
    else {
        fig.css(collectionCSS);
    }
    fig.append(slotSVG);

    // set name, id, and title
    fig.find('#name').text(slot.name);
    fig.attr('id',id);
    fig.attr('title',slot.desc);

    // open object editor for the object in the slot
    function edit() {
        var editor;
        if (fig.hasClass('filled')) {
            editor = new openmdao.ObjectFrame(project, pathname);
        }
        else {
            openmdao.Util.notify('Slot is empty!');
        }
    }

    // remove the contents of the slot
    function empty() {
        var cmd;
        if (fig.hasClass('filled')) {
            if (slot.containertype === 'list') {
                cmd = 'del '+pathname;
            }
            else if (slot.containertype === 'dict') {
                cmd = pathname + " = None";
            }
            else {
                cmd = openmdao.Util.getPath(pathname)
                    + ".remove('"+openmdao.Util.getName(pathname)+"')";
            }
            project.issueCommand(cmd);
        }
        else {
            openmdao.Util.notify('Slot is already empty!');
        }
    }

    // fill the slot
    function fill(newContents) {
        var cmd;
        if (slot.containertype === 'list') {
            if (pathname.lastIndexOf('[') >= 0) {
                // replace existing list item
                cmd = pathname + '=' + newContents;
            }
            else {
                cmd = pathname+'.append('+newContents+')';
            }
        }
        else {
            var slotParent = openmdao.Util.getPath(pathname);
            if (slot.filled) {
                cmd = slotParent+'.replace("'+slot.name+'", '+newContents+')';
            }
            else {
                if (slot.containertype === 'dict') {
                    cmd = pathname + '=' + newContents;
                }
                else {
                    cmd = slotParent+'.add("'+slot.name+'", '+newContents+')';
                }
            }
        }
        project.issueCommand(cmd);
    }

    // create context menu
    contextMenu.append(jQuery('<li title="Edit object">Edit</li>').click(function(e) {
        edit();
    }));
    contextMenu.append(jQuery('<li title="Remove object from slot">Remove</li>').click(function(e) {
        empty();
    }));
    ContextMenu.set(contextMenu.attr('id'), fig.attr('id'));

    /** open object editor on double click */
    fig.dblclick(function(e) {
        edit();
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
        tolerance: 'pointer',
        greedy: true,
        out: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOut(fig);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(fig);
        },
        drop: function(ev,ui) {
            var dropTarget = openmdao.drag_and_drop_manager.getDropTarget(ev, ui);
            if (dropTarget) {
                dropTarget.droppable('option', 'dropHandler')(ev, ui);
            }
        },
        dropHandler: function(ev,ui) {
            // could get same event multiple times if drop triggers for sibling targets
            if (this.dropEvent && this.dropEvent === ev.originalEvent) {
                return;  // already handled this drop event
            }
            this.dropEvent = ev.originalEvent;

            var droppedObject = jQuery(ui.draggable).clone(),
                droppedName = droppedObject.text(),
                droppedPath = droppedObject.attr("modpath");

            openmdao.drag_and_drop_manager.reset();

            openmdao.project.getSignature(droppedPath)
                .done(function(signature)  {
                    if (signature.args.length) {
                        var prompt = 'Enter arguments for new '+droppedName;
                        openmdao.Util.promptForArgs(prompt, signature, function(nm, args) {
                            fill('create("'+droppedPath+'"'+args+')');
                        }, true);
                    }
                    else {
                        fill('create("'+droppedPath+'")');
                    }
                })
                .fail(function(jqXHR, textStatus, errorThrown) {
                    debug.error('Error getting signature for', droppedPath,
                                jqXHR, textStatus, errorThrown);
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
            filled = (value !== null && value.length > 0);

        if (filled) {
            fig.addClass('filled');
            r.css(filledRectCSS);
            n.css(filledTextCSS);
            k.css(filledTextCSS).text(value.split(".").pop());
            fig.height(60);
            fig.width(100);
        }
        else {
            fig.removeClass('filled');
            r.css(unfilledRectCSS);
            n.css(unfilledTextCSS).text(slot.name);
            k.css(unfilledTextCSS).text(slot.klass);
            fig.height(60);
            fig.width(100);
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

    // set initial state
    this.setState(slot.filled);

    // resize it per user preference
    if (openmdao.preferences.SlotFigure.resize) {
        this.resize();
    }
};

/***********************************************************************
 * SlotListFigure: A graphical representation of a list of slots.
 *
 * A list of slots is represented as a comma separated list of
 * singleton slot figures in square brackets.
 *
 *  Arguments:
 *      elm:      the parent element in the DOM for this figure
 *      project:    object that provides access to the openmdao project
 *      pathname: the pathname of the slot
 *      slot:     the contents of the slot list
 ***********************************************************************/

openmdao.SlotListFigure=function(elm, project, pathname, slot) {
    var lbrktSVG = '<svg height="60" width="25">'
                 + '    <text x="0" y="48" font-size="60" style="fill:gray">[</text>'
                 + '</svg>',
        commaSVG = '<svg height="60" width="20">'
                 + '    <text x="0" y="48" font-size="60" style="fill:gray">,</text>'
                 + '</svg>',
        rbrktSVG = '<svg height="60" width="25">'
                 + '    <text x="0" y="48" font-size="60" style="fill:gray">]</text>'
                 + '</svg>',
        contDiv  = jQuery('<div style="width:100%;margin:10px;">'),
        listDiv  = jQuery('<div style="margin:0px; clear:both;">'),
        slotCnt  = Object.keys(slot.filled).length,
        slotName = openmdao.Util.getName(pathname),
        counter  = 0;

    elm.append(contDiv);
    contDiv.append(listDiv);

    listDiv.append(lbrktSVG);

    // ad a slot figure for each item in the list
    jQuery.each(slot.filled, function(idx, val) {
        var slot_info = {
            containertype: "list",
            desc: slot.desc,
            filled: val,
            klass: val ? val : slot.klass,
            name: slotName+'['+idx+']'
        };

        openmdao.SlotFigure(listDiv, project, pathname+'['+idx+']', slot_info);

        listDiv.append(commaSVG);
        counter = counter + 1;
    });

    // add an empty slot to the end of the list as a drop target
    openmdao.SlotFigure(listDiv, project, pathname, {
        containertype: "list",
        desc: slot.desc,
        filled: false,
        klass: slot.klass,
        name: slotName
    });

    listDiv.append(rbrktSVG);
};


/***********************************************************************
 * SlotDictFigure: A graphical representation of a dictionary of slots.
 *
 * A dictionary of slots is represented as a comma separated list of
 * singleton slot figures in curly brackets.
 *
 *  Arguments:
 *      elm:      the parent element in the DOM for this figure
 *      project:    object that provides access to the openmdao project
 *      pathname: the pathname of the slot
 *      slot:     the contents of the slot dictionary
 ***********************************************************************/

openmdao.SlotDictFigure=function(elm, project, pathname, slot) {
    var lcrlySVG = '<svg height="60" width="30">'
                 + '    <text x="0" y="48" font-size="60" style="fill:gray">{</text>'
                 + '</svg>',
        commaSVG = '<svg height="60" width="20">'
                 + '    <text x="0" y="48" font-size="60" style="fill:gray">,</text>'
                 + '</svg>',
        rcrlySVG = '<svg height="60" width="30">'
                 + '    <text x="0" y="48" font-size="60" style="fill:gray">}</text>'
                 + '</svg>',
        contDiv  = jQuery('<div style="width:100%;margin:10px;">'),
        nameDiv  = jQuery('<div style="color:gray;margin:0px">'
                 + openmdao.Util.getName(pathname)+':</div>'),
        dictDiv  = jQuery('<div style="margin:0px; clear:both;">'),
        slotCnt  = Object.keys(slot.filled).length;

    // don't bother if there are no slots in the dict (nothing to see)
    if (slotCnt > 0) {
        elm.append(contDiv);
        contDiv.append(nameDiv);
        contDiv.append(dictDiv);

        dictDiv.append(lcrlySVG);

        jQuery.each(slot.filled, function(key, val) {
            var slot_info = {
                containertype: "dict",
                desc: slot.desc + ": " + key,
                filled: val,
                klass: val ? val : slot.klass,
                name: key
            };

            openmdao.SlotFigure(dictDiv, project, pathname+'["'+key+'"]', slot_info, true);

            if (slotCnt > 1) {
                dictDiv.append(commaSVG);
            }
            slotCnt = slotCnt - 1;
        });

        dictDiv.append(rcrlySVG);
    }
};
