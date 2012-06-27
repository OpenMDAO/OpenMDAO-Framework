
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PaletteFrame = function(id,model) {
    openmdao.PaletteFrame.prototype.init.call(this,id,'Library',[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        palette = jQuery('#'+id),
        libs = jQuery('<div>').appendTo(palette);

    /** find the actual top of the given element, taking visibility and 
        scrolling into account */
    function getElementTop(elem) {
       var yPos = 0;
       var scrolls = 0;
       var firstElemWithOSP = 0;
       while(elem && !isNaN(elem.offsetTop)) {
          scrolls += elem.scrollTop;
          if (firstElemWithOSP === 0 && elem.offsetParent) {
             firstElemWithOSP = elem;
          }
          elem = elem.parentNode;
       }
       
       elem = firstElemWithOSP;
       while(elem && !isNaN(elem.offsetTop)) {
          yPos += elem.offsetTop;
          elem = elem.offsetParent;
       }
       return yPos-scrolls;
    }
    

    /** rebuild the Palette from a JSON library list of tuples of the form (libname, meta_dict) */
    function updatePalette(packages) {
        // build the new html
        var html="<div id='library'>";
        html += '<div class="ui-widget"><label for="objtt-select" id="objtt-search">Search: </label><input id="objtt-select"></div>';
        html+= '<table cellpadding="0" cellspacing="0" border="0" id="objtypetable">';
        // headers: ClassName, Module Path, Version, Context, Interfaces
        html += '<thead><tr><th></th><th></th><th></th><th></th><th></th></tr></thead><tbody>';
        jQuery.each(packages, function(name,item) {
            html+= packageHTML(name, item);
        });
        html+="</tbody></table></div>";

        // replace old html
        libs.html(html);

        var dtable = palette.find('#objtypetable').dataTable({
            'bPaginate': false,
            'bjQueryUI': true,
            'sScrollY': '600px',
            'bScrollCollapse': true,
            'bFilter': true,    // make sure filtering is still turned on
            'aoColumnDefs': [
                 { 'bVisible': false, 'aTargets': [1,2,3,4] }
             ],
            'sDom': '<"H"lr>t<"F">'   // removes the built-in filter field and bottom info (default is lfrtip)
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
                    //"UncertainVariable",
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
               // elemoffsetHeight = 0 for invisible entries
               if (otop <= event_top && (otop+elem.offsetHeight)>=event_top) {
                  match = elem;
                  return false; // break out of loop
               }
            });
            return match;
        }
        
        contextMenu.append(jQuery('<li>View Docs</li>').click(function(ev) {
            debug.info('View Docs context event:');
            var modpath = _findMatch(ev).getAttribute('modpath');
            var url = '/docs/plugins/'+modpath;
            var parts = modpath.split('.')
            var cname = parts.pop()
            window.open(url, 'Docs for '+modpath);
        }));
        contextMenu.append(jQuery('<li>View Metadata</li>').click(function(ev) {
            debug.info('View Metadata context event:');
            var match = _findMatch(ev);
            var win = jQuery('<div></div>');
            var table = jQuery('<table cellpadding=5px>');
            table.append('<tr><th></th><th></th></tr>')
            var hdata = ['name','modpath','version','context','ifaces'];
            var data = dtable.fnGetData(match.parentNode);
            for (var i=1; i<data.length; i++) {
               table.append('<tr><td>'+hdata[i]+'</td><td>'+data[i]+'</td></tr>');
            }
            win.append(table);
            
            // Display dialog
            jQuery(win).dialog({
                title: match.innerText+' Metadata',
                width: 'auto'
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
    model.addListener('types', handleMessage);

    this.update();

};

/** set prototype */
openmdao.PaletteFrame.prototype = new openmdao.BaseFrame();
openmdao.PaletteFrame.prototype.constructor = openmdao.PaletteFrame;

