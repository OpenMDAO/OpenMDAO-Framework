/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source Agreement (NOSA)

*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.Menu = function(id, json) {
    /***********************************************************************
     *  private
     ***********************************************************************/
    var self = this,
        elm = jQuery("#"+id)
        
    /** build menus from JSON data structure */
    function buildMenus(menus) {
        // generate HTML for the menus
        var html = "<ul>"
        for (var i = 0; i < menus.length; i++) {
            html += getMenuHTML(menus[i])
        }
        html += "</ul>"
        elm.html(html);

        // add indicators and hovers to submenu parents
        elm.find("li").each(function() {
            if (jQuery(this).find("ul").length > 0) {
                jQuery("<span>").text("^").appendTo(jQuery(this).children(":first"));

                // show subnav on hover
                jQuery(this).mouseenter(function() {
                  jQuery(this).find("ul").stop(true, true).slideDown();
                });

                // hide submenus on exit
                jQuery(this).mouseleave(function() {
                  jQuery(this).find("ul").stop(true, true).slideUp();
                });
            }
        });
    }
    
    /** recursively build HTML for JSON nested menu structure */
    function getMenuHTML(menu) {
        var menuHTML = '<li><a '
        if ('url' in menu) {
            menuHTML += 'href="'+menu.url+'" '
        }
        if ('onclick' in menu) {
            menuHTML += 'onclick="'+menu.onclick+'" '
        }
        menuHTML += '>'+menu.text+'</a>'
        
        if ('items' in menu) {
            menuHTML += "<ul>"
            for (var i = 0; i < menu.items.length; i++) {
                menuHTML += getMenuHTML(menu.items[i])
            }
            menuHTML += "</ul>"
        }
        
        menuHTML += '</li>'
        return menuHTML;
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** rebuild menus from given JSON data */
    this.updateFromJSON = function(json) {
        buildMenus(json)
    }
    
    /** rebuild menus from JSON at given url */
    this.updateFromURL = function(url) {
        jQuery.ajax({
            type: 'GET',
            url: url,
            dataType: 'json',
            success: buildMenus,
            error: function(x,y,z) { debug.info("Error getting Menu data:",x,y,z) }
        })
    }
    
    buildMenus(json)
}
