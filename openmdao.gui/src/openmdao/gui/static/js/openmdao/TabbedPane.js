/**
 * a tabbed pane based on a definition list
 */

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.TabbedPane = function(id) {
    jQuery("#"+id+" dd").hide();
    jQuery("#"+id+" dt").click(function() {
        var tgt = jQuery(this).attr("target");
        jQuery("#"+id+" dd:visible").hide();
        jQuery("#"+id+" dt.tab_here").removeClass("tab_here");
        jQuery("#"+tgt).show();
        jQuery(this).addClass("tab_here");
    });
}
