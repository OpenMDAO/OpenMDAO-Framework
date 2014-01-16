/* This pane appears on drivers that have the HasEvents interface. */

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.TriggersPane = function(elm, project, pathname, name) {
    var events,
        eventsDiv = jQuery("<div id='"+name+"_triggers' class='slickgrid' style='overflow:none; height:320px; width:620px'>"),
        addButton = jQuery("<button>Add Event</button>").button(),
        clrButton = jQuery("<button>Clear Events</button>").button(),
        columns = [
            {id:"del",     name:"",        field:"del",     width:25, formatter:buttonFormatter},
            {id:"target",  name:"Target",  field:"target",  width:500}
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: false,
            autoEdit: false
        };

    function buttonFormatter(row, cell, value, columnDef, dataContext) {
        button = '<div class="ui-icon-trash"></div>';
        return button;
    }
    elm.append(eventsDiv);

    var tabdiv = jQuery('<div class="post_slick" style="height:40px;">'),
        table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    tabdiv.append(table);
    elm.append(tabdiv);

    events = new Slick.Grid(eventsDiv, [], columns, options);
    events.onClick.subscribe(function(e) {
        var cell = events.getCellFromEvent(e);
        if (cell.cell === 0) {
            var delname = events.getData()[cell.row].target,
                cmd = pathname+'.remove_event("'+delname+'")';
            project.issueCommand(cmd);
        }
    });

    eventsDiv.bind('resizeCanvas', function() {
        events.resizeCanvas();
    });

    /** add a new event */
    function addEvent(target) {
        var cmd = pathname+".add_event('"+target+"')";
        project.issueCommand(cmd);
    }

    /** prompt for new event */
    function promptForEvent(callback) {

        var candidates = [];

        project.getAvailableEvents(pathname).done(function(cjson) {
            candidates = cjson;

            // Build dialog markup
            var win = jQuery('<div id="event-dialog"></div>'),
                target = jQuery('<input id="event-target" type="text" style="width:100%"></input>');

            win.append(jQuery('<div>Target: </div>').append(target));
            event_selector = win.find('#event-target');

            // update the event selector.
            event_selector.html('');
            event_selector.autocomplete({ source: candidates, minLength:0});

            function handleResponse(ok) {
                win.dialog('close');
                if (ok) {
                    callback(target.val());
                }
                win.remove();
            }

            function setupSelector(selector) {

                // process new selector value when selector loses focus
                selector.bind('blur', function(e) {
                    selector.autocomplete('close');
                });

                // set autocomplete to trigger blur (remove focus)
                selector.autocomplete({
                    select: function(event, ui) {
                        selector.val(ui.item.value);
                        selector.blur();
                    },
                    delay: 0,
                    minLength: 0
                });

                // set enter key to trigger blur (remove focus)
                selector.bind('keypress.enterkey', function(e) {
                    if (e.which === 13) {
                        selector.blur();
                        handleResponse(true);
                    }
                });
            }

            setupSelector(event_selector);

            // Display dialog
            jQuery(win).dialog({
                modal: true,
                title: 'New Event',
                buttons: [
                    {
                        text: 'Ok',
                        id: 'event-ok',
                        click: function() { handleResponse(true); }
                    },
                    {
                        text: 'Cancel',
                        id: 'event-cancel',
                        click: function() { handleResponse(false); }
                    }
                ]
            });
        });
    }

    /** clear all events */
    function clearEvents() {
        var cmd = pathname+".clear_events()";
        project.issueCommand(cmd);
    }

    addButton.click(function() { promptForEvent(addEvent); });
    clrButton.click(function() { clearEvents(); });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            events.setData(properties);
        }
        else {
            events.setData([]);
            alert('Error getting properties for '+pathname+' ('+name+')');
        }
        events.resizeCanvas();
    };
};

