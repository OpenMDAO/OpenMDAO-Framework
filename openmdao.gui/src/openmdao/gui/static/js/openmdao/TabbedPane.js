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
    jQuery("#"+id+" dl").css({
        'margin': '0',
        'width': '100%',
        'position': 'relative'
    });
    jQuery("#"+id+" dt").css({
        'top':'0', 
        'margin': '0', 
        'position': 'relative', 
        'float': 'left',
        'display': 'block',
        'width': '75px',
        'height': '20px',
        'text-align': 'center',
        'border': '1px solid #222', 
        'background-color':'#6a6a6a',
        'color': 'white',
        'font-size': '14px'
    });
    jQuery("#"+id+" dd").css({
        'margin': '0',
        'position': 'absolute',
        'left': '0',
        'top': '25px',
        'height': '100%',
        'width': '100%'
    });
    
    jQuery("#"+id+" dd").hide();
    jQuery("#"+id+" dt").click(function() {
        var tgt = jQuery(this).attr("target");
        jQuery("#"+id+" dd:visible").hide();
        jQuery("#"+id+" dt.tab_here").removeClass("tab_here");
        jQuery("#"+tgt).show();
        jQuery(this).addClass("tab_here");
    });
}
