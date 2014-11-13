
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DrawingPane = function(elm, project, pathname, name) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        css = 'height:100%; width:100%;', //position:relative;',
        drawing = jQuery('<div id="'+name+'" style="'+css+'";>')
           .appendTo(elm);

    elm.css({ 'overflow':'auto' });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the Drawing with SVG data */
    this.loadData = function(svg_data) {
        // TODO: check that it's valid SVG data
        if (svg_data) {
            drawing.html(svg_data);
        }
        else {
            drawing.html('Drawing not available');
        }
    };
};
