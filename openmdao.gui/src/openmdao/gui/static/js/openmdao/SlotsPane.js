
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

    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            debug.info('SlotsPane.updateFigures() slot',idx,slot);
            if (figures[slot.name]) {
                figures[slot.name].setState(slot.klass, slot.filled);
            }
            else {
                var fig = openmdao.SlotFigure(model, pathname+'.'+slot.name,
                                              slot.containertype, slot.klass,
                                              slot.desc, slot.filled);
                figures[name] = fig;
                slotsDiv.append(fig);
            }
        });
    }

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
