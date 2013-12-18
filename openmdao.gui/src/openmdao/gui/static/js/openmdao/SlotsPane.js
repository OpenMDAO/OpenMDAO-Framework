/***********************************************************************
 *  SlotsPane: A pane that shows a collection of slots.
 *
 * Slots are rendered as rows of dash-outlined component-like figures.
 *
 *  Arguments:
 *      elm:      the parent element in the DOM for this pane
 *      project:  object that provides access to the openmdao project
 *      pathname: the pathname of the object containing the Slots
 *      name:     not used
 *      editable: not used
 ***********************************************************************/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotsPane = function(elm, project, pathname, name, editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        slotsID = pathname.replace(/\./g,'-')+"-slots",
        slotsDiv = jQuery('<div style="position:relative; background-color:black;">')
            .appendTo(elm),
        contextMenu = jQuery("<ul id="+slotsID+"-menu class='context-menu'>")
            .appendTo(elm),
        slotsData = {};

    self.pathname = pathname;

    elm.css({'overflow':'auto'});

    contextMenu.append(jQuery('<li title="Toggle autosizing of slots">Toggle Autosizing</li>').click(function(e) {
        openmdao.preferences.SlotFigure.resize = ! openmdao.preferences.SlotFigure.resize;
        self.loadData(slotsData);
    }));
    ContextMenu.set(contextMenu.attr('id'), elm.attr('id'));

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

        jQuery.each(json, function(idx, slot) {
            var slotName = pathname+'.'+slot.name;
            if (slot.containertype === 'dict') {
                openmdao.SlotDictFigure(slotsDiv, project, slotName, slot);
            }
            else if (slot.containertype === 'list') {
                openmdao.SlotListFigure(slotsDiv, project, slotName, slot);
            }
            else {
                openmdao.SlotFigure(slotsDiv, project, slotName, slot);
            }
        });
    }

    // this is just to prevent drops from falling thru to underlying panes
    elm.droppable ({
        accept: '.objtype',
        tolerance: 'pointer',
        out: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOut(elm);
        },
        over: function(ev,ui) {
            openmdao.drag_and_drop_manager.draggableOver(elm);
        },
        drop: function(ev,ui) {
            var dropTarget = openmdao.drag_and_drop_manager.getDropTarget(ev, ui);
            // getting some intermittent errors here.. catch and log a warning
            try {
                dropTarget.droppable('option', 'dropHandler')(ev,ui);
            }
            catch (err) {
                debug.warn('Unexpected error in openmdao.SlotsPane.droppable',
                           'dropTarget:', dropTarget);
            }
        },
        dropHandler: function(ev,ui) {
        }
    });

    /***********************************************************************
     *  protected
     ***********************************************************************/

    /** update slots diagram */
    this.loadData = function(json) {
        slotsData = json;
        slotsDiv.html('');
        if (Object.keys(json).length > 0) {
            updateFigures(slotsData);
        }
    };
};
