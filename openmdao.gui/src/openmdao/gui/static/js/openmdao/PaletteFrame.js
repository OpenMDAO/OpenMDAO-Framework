
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PaletteFrame = function(id,model) {
    openmdao.PaletteFrame.prototype.init.call(this,id,'Libraries',[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        palette = jQuery('#'+id),
        libs = jQuery('<div>').appendTo(palette);

    // dropping a filename onto the palette pane means import *
    /*
    libs.droppable ({
        accept: '.file',
        drop: function(ev,ui) {
            debug.info('PaletteFrame drop: ',ev,ui);
            var droppedObject = jQuery(ui.draggable).clone();
            debug.info('PaletteFrame drop: ',droppedObject);
            var path = droppedObject.attr("path");
            debug.info('PaletteFrame drop: '+path);
            if (/.py$/.test(path)) {
                model.importFile(path);
            }
            else {
                alert("Not a python file:\n"+path);
            }
        }
    });
    */
    function getElementTop(elem) {
       var yPos = 0;
       var scrolls = 0;
       var firstElemWithOSP = 0;
       while(elem && !isNaN(elem.offsetTop)) {
          //if (elem.scrollTop) {
             //debug.info('<'+elem.tagName+'> scrollTop: '+elem.scrollTop);
          //}
          scrolls += elem.scrollTop;
          if (firstElemWithOSP === 0 && elem.offsetParent) {
             firstElemWithOSP = elem;
             //debug.info('firstOSP');
          }
          elem = elem.parentNode;
       }
       
       elem = firstElemWithOSP;
       while(elem && !isNaN(elem.offsetTop)) {
          //debug.info('<'+elem.tagName+'> offsetTop: '+elem.offsetTop);
          yPos += elem.offsetTop;
          elem = elem.offsetParent;
       }
       return yPos-scrolls;
    }
    
    /** rebuild the Palette from a JSON library list of tuples of the form (libname, meta_dict) */
    function updatePalette(packages) {
        // build the new html
        var html="<div id='library'>";
        html+= '<table cellpadding="0" cellspacing="0" border="0" id="objtypetable">';
        // headers: Class, Module Path, Version, Interfaces
        html += '<thead><tr><th>Name</th><th>ModulePath</th><th>Version</th><th>Context</th><th>Interfaces</th></tr></thead><tbody>';
        html += '<div class="ui-widget"><label for="objtt-select" id="objtt-search">Search: </label><input id="objtt-select"></div>';
        jQuery.each(packages, function(name,item) {
            html+= packageHTML(name, item);
        });
        html+="</tbody></table></div>";

        // replace old html
        libs.html(html);
        
        var dtable = palette.find('#objtypetable').dataTable({
            'bPaginate': false,
            'bjQueryUI': true,
            'sScrollY': '500px',
            'bScrollCollapse': true,
            'bFilter': true,    // make sure filtering is still turned on
            'aoColumnDefs': [
                 { 'bVisible': false, 'aTargets': [1,2,3,4] }
             ],
            'sDom': 'lrtp'   // removes the built-in filter field and bottom info (default is lfrtip)
        });
        
        // here's the default list of filters for the library
        var selections = [
                    "In Project",
                    "Architecture",
                    "Assembly",
                    "CaseRecorder",
                    "CaseIterator",
                    "Component",
                    "Differentiator",
                    "DOEgenerator",
                    "Driver",
                    "Solver",
                    "Surrogate",
                    "UncertainVariable",
                    "Variable"
                ];
        var input_obj = palette.find('#objtt-select');
        input_obj.autocomplete({
           source: function(term, response_cb) {
               response_cb(selections);
           },
           select: function(event, ui) {
               input_obj.value = ui.item.value;
               ent = jQuery.Event('keypress.enterkey');
               ent.target = input_obj;
               ent.which = 13;
               input_obj.trigger(ent);
           },
           delay: 0,
           minLength: 0
        });
        input_obj.bind('keypress.enterkey', function(e) {
            if (e.which === 13) {
                dtable.fnFilter( e.target.value );
                if (selections.indexOf(e.target.value) === -1) {
                   selections.push(e.target.value);
                }
                input_obj.autocomplete('close');
            }
        });
        
        var contextMenu = jQuery("<ul id='lib-cmenu' class='context-menu'>")
                          .appendTo(dtable);

        var objtypes = dtable.find('.objtype');
        
        // given a click event, find the table entry that corresponds
        // to that location. Returns the matching element.
        function _findMatch(ev) {
            var otop = 0, match=0;
            var event_top = ev.target.offsetParent.offsetTop;
            objtypes.each(function(i, elem) {
               otop = getElementTop(elem);
               if (elem.offsetHeight > 0 && otop <= event_top && (otop+elem.offsetHeight)>=event_top) {
                  match = elem;
                  return false; // break out of loop
               }
               //if (otop >= event_top) {
                  //match = objtypes[i-1];
                  //return false; // break out of loop
               //}
            });
            //if (match===0 && event_top >= otop) { // check for last entry in table
               //match = objtypes[objtypes.length-1];
            //}
            return match;
        }
        
        contextMenu.append(jQuery('<li>View Docs</li>').click(function(ev) {
            debug.info('View Docs context event:');
            debug.info('match is: '+_findMatch(ev).getAttribute('modpath'));
            debug.info(ev);
        }));
        contextMenu.append(jQuery('<li>View Metadata</li>').click(function(ev) {
            debug.info('View Metadata context event:');
            debug.info('match is: '+_findMatch(ev).getAttribute('modpath'));
            debug.info(ev);
            var match = _findMatch(ev);
            var win = jQuery('<div></div>');
            var table = jQuery('<table>');
            var hrow = jQuery('<tr></tr>');
            dtable.find('th').each(function() {
               debug.info("th "+this.innerText);
               hrow.append('<th>'+this.innerText+'</th>');
            });
            var row = jQuery('<tr></tr>');
            jQuery(match.parentNode).find('td').each(function() {
               debug.info('td '+this.innerText);
               row.append('<td>'+this.innerText+'</td>');
            });
            debug.info('table');
            debug.info(table);
            table.append(hrow);
            table.append(row);
            win.append(table);
            
            // Display dialog
            jQuery(win).dialog({
                'title': 'Metadata for '+match.innerText,
            });
        }));
        ContextMenu.set(contextMenu.attr('id'), dtable.attr('id'));
        
        // make everything draggable
        objtypes.draggable({ helper: 'clone', appendTo: 'body' });
        objtypes.addClass('jstree-draggable'); // allow drop on jstree
    }

    /** build HTML string for a package */
    function packageHTML(name,item) {
        var html = "<tr><td class='objtype' modpath="+item.modpath+">"+name+"</td><td>"+
                   item.modpath+"</td><td>"+item.version+"</td><td>"+
                   item._context+"</td><td>"+item.ifaces+"</td></tr>";
        return html;
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== 'types') {
            debug.warn('Invalid types data:',message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            libs.html("<div>Updating...</div>")
                .effect('highlight',{color:'#ffd'},1000);
            updatePalette(message[1][0]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the display, with data from the model */
    this.update = function() {
        libs.html("<div>Updating...</div>")
            .effect('highlight',{color:'#ffd'},1000);
        model.getTypes(updatePalette);
    };

    // ask model for an update whenever something changes
    model.addListener('types',handleMessage);
    
    this.update();

};

/** set prototype */
openmdao.PaletteFrame.prototype = new openmdao.BaseFrame();
openmdao.PaletteFrame.prototype.constructor = openmdao.PaletteFrame;

