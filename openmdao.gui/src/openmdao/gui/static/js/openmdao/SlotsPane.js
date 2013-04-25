
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

    /** update slots by recreating figures from JSON slots data */
    function updateFigures(json) {

        // Sort slots by name
        json.sort(function(a, b) {
            var nameA=a.name.toLowerCase(), nameB=b.name.toLowerCase() ;
            if (nameA < nameB) {  //sort string ascending
                return -1;
            }
            if (nameA > nameB) {
                return 1;
            }
            return 0;
        }) ;

        jQuery.each(json, function(idx,slot) {
            if (figures[slot.name]) {
                // update existing slot figure
                figures[slot.name].setState(slot.filled);
            }
            else {
                if (slot.containertype === 'dict') {
                    // create a new slot figure for each item in the dict
                    // Need to build a new slot object for the specific item in the dict

                    var lcrlySVG = '<svg height="60" width="35">'
                                 + '    <text x="0" y="45" font-size="60" style="fill:gray">{</text>'
                                 + '</svg>',
                        commaSVG = '<svg height="60" width="20">'
                                 + '    <text x="0" y="45" font-size="60" style="fill:gray">,</text>'
                                 + '</svg>',
                        rcrlySVG = '<svg height="60" width="30">'
                                 + '    <text x="0" y="45" font-size="60" style="fill:gray">}</text>'
                                 + '</svg>',
                        dictDiv  = '<div style="margin:10px; clear:both;" />';

                    slotsDiv.append(dictDiv);
                    slotsDiv.append(lcrlySVG);
                    jQuery.each(slot.filled, function( idx, slot_in_dict_info) {
                        var dict_key = slot_in_dict_info[ "py/tuple" ][0],
                            dict_value = slot_in_dict_info[ "py/tuple" ][1];
                        if (dict_value) {
                            dict_value = dict_value[ "py/object" ] ;
                        }
                        var slot_in_dict = {
                            containertype: "singleton",
                            desc: slot.desc + " for " + dict_key,
                            filled: dict_value,
                            klass: "ISurrogate",
                            name: dict_key
                        };
                        var fig = openmdao.SlotFigure(model, pathname+'.'+slot.name+"." + dict_key,
                                                      slot_in_dict, true ),
                        figMenu = fig.getContextMenu();

                        var options = {} ;

                        figures[slot.name+"."+dict_key] = fig;

                        slotsDiv.append(fig);

                        // It is only at this point where we can get at the
                        // size of the text in the slots so that the
                        // bounding oval can be resized
                        var name_text = fig.find( '#name' )[0] ;
                        var klass_text = fig.find( '#klass' )[0] ;
                        var name_width = name_text.getBBox().width ;
                        var klass_width = klass_text.getBBox().width ;
                        var rect_width = Math.max( name_width, klass_width ) ;
                        if ( rect_width === 0 ) {
                            rect_width = 100;
                        }
                        var svg = fig.find( "svg" )[0] ;
                        var rect = fig.find( "rect" )[0] ;
                        svg.setAttribute( "width", rect_width + 40 ) ;
                        rect.setAttribute( "width", rect_width + 40 ) ;
                        name_text.setAttribute( "x", ( rect_width + 40 ) / 2 ) ;
                        klass_text.setAttribute( "x", ( rect_width + 40 ) / 2 ) ;
                        fig.width( rect_width + 45 ) ;

                        if ( idx < slot.filled.length - 1 ) {
                            slotsDiv.append(commaSVG);
                        }
                        ContextMenu.set(figMenu.attr('id'), fig.attr('id'));
                    } ) ;
                    slotsDiv.append(rcrlySVG);
                }
                else {
                    // create a new slot figure
                    var fig = openmdao.SlotFigure(model, pathname+'.'+slot.name, slot, false),
                    figMenu = fig.getContextMenu();
                    figures[slot.name] = fig;
                    slotsDiv.append(fig);
                    // It is only at this point where we can get at the
                    // size of the text in the slots so that the
                    // bounding oval can be resized
                    if ( slot.containertype === 'singleton' ) {
                        var name_text = fig.find( '#name' )[0] ;
                        var klass_text = fig.find( '#klass' )[0] ;
                        var name_width = name_text.getBBox().width ;
                        var klass_width = klass_text.getBBox().width ;
                        var rect_width = Math.max( name_width, klass_width ) ;
                        if ( rect_width === 0 ) {
                            rect_width = 100;
                        }
                        var svg = fig.find( "svg" )[0] ;
                        var rect = fig.find( "rect" )[0] ;
                        svg.setAttribute( "width", rect_width + 40 ) ;
                        rect.setAttribute( "width", rect_width + 40 ) ;
                        name_text.setAttribute( "x", ( rect_width + 40 ) / 2 ) ;
                        klass_text.setAttribute( "x", ( rect_width + 40 ) / 2 ) ;
                    }
                    ContextMenu.set(figMenu.attr('id'), fig.attr('id'));
                }
            }
        });
    }

    // this is just to prevent drops from falling thru to underlying panes
    elm.droppable ({
        accept: '.objtype',
        out: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOut(elm);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(elm);
        },
        drop: function(ev,ui) {
            var top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
            if (top_div) {
                var drop_function = top_div.droppable('option', 'actualDropHandler');
                drop_function(ev,ui);
            }
        }
    });

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
