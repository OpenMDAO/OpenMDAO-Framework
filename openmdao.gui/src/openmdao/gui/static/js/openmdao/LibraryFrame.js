
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.LibraryFrame = function(id, project) {
    openmdao.LibraryFrame.prototype.init.call(this,id,'Library',[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this;

    /** find the actual top of the given element, taking visibility and
        scrolling into account */
    function getElementTop(elem) {
       var yPos = 0,
           scrolls = 0,
           firstElemWithOSP = 0;

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

    /** rebuild the Library from a JSON library list of tuples of the form:
        (libname, meta_dict) */
    function updateLibrary(packages) {
        // build the new html
        var html = '<div class="ui-widget" style="clear:both">'
                 +   '<label for="objtt-filter" id="objtt-search">Search: </label>'
                 +   '<table id="objtt-group"><tr>'
                 +     '<td><input id="objtt-filter"></td>'
                 +     '<td><button id="objtt-clear">X</button></td>'
                 +   '</tr></table>'
                 + '</div>';
        html += '<table id="objtypetable" style="width:100%"' +
                ' cellpadding="0" cellspacing="0" border="0" >';
        // headers: ClassName, Module Path, Version, Context, Interfaces
        html += '<thead><tr><th></th><th></th><th></th><th></th><th></th></tr></thead>';
        html += '<tbody>';
        jQuery.each(packages, function(name,item) {
            html+= packageHTML(name, item);
        });
        html += '</tbody></table>';

        // replace old html
        self.elm.html(html);

        var dtable = self.elm.find('#objtypetable').dataTable({
            'bPaginate': false,
            'bjQueryUI': true,
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
                    "DOEgenerator",
                    "Driver",
                    "Optimizer",
                    "Solver",
                    "Surrogate"
                ];
        var input_obj = self.elm.find('#objtt-filter');

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
           open: function(event, ui) {
               self.searchListOpen = true;
           },
           close: function(event, ui) {
               self.searchListOpen = false;
           },
           delay: 0,
           minLength: 0
        });

        input_obj.bind('keypress.enterkey', function(e) {
            if (e.which === 13) {
                input_obj.autocomplete('close');
                dtable.fnFilter(e.target.value);
                dtable.width('100%');
                var found = jQuery('#objtypetable > tbody > tr > td');
                if (found.length > 1 ||
                    found.attr('class') !== 'dataTables_empty') {
                    if (selections.indexOf(e.target.value) === -1) {
                        selections.push(e.target.value);
                    }
                }
            }
        });

        var clrButton = self.elm.find('#objtt-clear');
        clrButton.click(function() {
            inputObj = self.elm.find('#objtt-filter');
            inputObj.val('');
            if (!self.searchListOpen) {
                dtable.fnFilter('');
                dtable.width('100%');
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
            var modpath = _findMatch(ev).getAttribute('modpath'),
                url     = '/docs/plugins/'+modpath,
                parts   = modpath.split('.'),
                cname   = parts.pop();
            window.open(url, 'Docs for '+modpath);
        }));
        contextMenu.append(jQuery('<li>View Metadata</li>').click(function(ev) {
            var match = _findMatch(ev),
                win   = jQuery('<div></div>'),
                table = jQuery('<table cellpadding=5px>')
                          .append('<tr><th></th><th></th></tr>'),
                hdata = ['name','modpath','version','context','ifaces'],
                data  = dtable.fnGetData(match.parentNode),
                i;
            for (i=1; i<data.length; i++) {
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
        objtypes.draggable({
            appendTo: 'body',
            opacity: 0.5,
            helper: function() {
                // a helper that looks like a semi-transparent object
                var html = '<div class="ObjTypeHelper">'
                         + '  <svg height="60" width="100">'
                         + '    <rect x="0" y="5" height="50" width="100" rx="15" ry="15" style="fill:gray;stroke-width:2;stroke:black;" />'
                         + '    <text id="klass" x="50" y="33" font-style="italic" text-anchor="middle">'+this.innerText+'</text>'
                         + '  </svg>'
                         + '</div>';
                return jQuery(html);
            },
            cursorAt: {left: 0, top: 0}
        });
        // TODO: Could not get this to work if the cursor was for .objtype in mdao-style.css
        //        For some reason this does not override that during the drag
        //        But for now it all looks pretty good with just an open hand for the hover
        //        and then drag
        //cursor: 'url( http://www.google.com/intl/en_ALL/mapfiles/closedhand.cur ) 8 8, auto ' });

        //objtypes.addClass('jstree-draggable'); // allow drop on jstree
    }

    /** build HTML string for a package */
    function packageHTML(name,item) {
        var classes = item.ifaces.toString().replace(/,/g," ")+" ";

        if (item.hasOwnProperty('bases')) {
            classes += item.bases.toString().replace(/,/g," ");
        }

        return "<tr><td class='objtype " + classes + "' modpath="+item.modpath+">"+name+"</td>"+
                   "<td>"+item.modpath+"</td><td>"+item.version+"</td>"+
                   "<td>"+item._context+"</td><td>"+item.ifaces+"</td>"+
               "</tr>";
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== 'types') {
            debug.warn('Invalid types data:',message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            self.elm.html("<div>Updating...</div>")
                .effect('highlight',{color:'#ffd'},1000);
            updateLibrary(message[1][0]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the display, with data from the project */
    this.update = function() {
        self.elm.html("<div>Updating...</div>")
            .effect('highlight',{color:'#ffd'},1000);
        project.getTypes().done(updateLibrary);
    };

    // ask project for an update whenever something changes
    project.addListener('types', handleMessage);

    // initial update
    project.project_ready.always(function() {
        self.update();
    });
};

/** set prototype */
openmdao.LibraryFrame.prototype = new openmdao.BaseFrame();
openmdao.LibraryFrame.prototype.constructor = openmdao.LibraryFrame;

