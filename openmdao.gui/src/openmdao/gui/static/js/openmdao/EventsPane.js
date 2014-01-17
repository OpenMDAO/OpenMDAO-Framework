/* This pane appears on components that have events. */

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.EventsPane = function(elm, project, pathname, name) {
    var self = this,
        events,
        eventsDiv = jQuery("<div id='"+name+"_events' class='slickgrid' style='overflow:none; height:320px; width:620px'>"),
        columns = [
            {id:"name",      name:"Name",        field:"name",      width:150 },
            {id:"desc",      name:"Description", field:"desc",      width:450 }
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: false,
            autoEdit: false
        };

    elm.append(eventsDiv);

    var tabdiv = jQuery('<div class="post_slick" style="height:40px;">'),
        table = jQuery('<table width="100%">');

    tabdiv.append(table);
    elm.append(tabdiv);

    events = new Slick.Grid(eventsDiv, [], columns, options);

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