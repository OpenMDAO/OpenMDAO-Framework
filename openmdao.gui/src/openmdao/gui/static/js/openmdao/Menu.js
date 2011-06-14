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
openmdao.Menu = function(id) {
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        elm = jQuery("#"+id),
        menus = [
            { text: "Project", 
              items: [
                { text: "Save",          onclick: "model.saveProject();" },
                { text: "Run",           onclick: "model.runModel();" },
                //{ text: "New",           onclick: "model.newModel()"   },
                { text: "New",           url: "/workspace/exit" },
                { text: "Logout",        url: "/workspace/logout" },
                //{ text: "Exit",          onclick: "model.exit();openmdao.Util.closeWindow()"}
              ]
            },
            { text: "File", 
              items: [
                { text: "New File",      onclick: "model.newFile();" },
                { text: "New Folder",    onclick: "model.newFolder();" },
                { text: "Add File",      onclick: "model.uploadFile();" },
              ]
            },
            { text: "View", 
              items: [
                { text: "Schematic",     url: "#" },
                { text: "Code Editor",   url: "#" },
                { text: "Command Line",  onclick: "openmdao.Util.toggle_visibility('cmdform');" },
                { text: "Refresh",       onclick: "model.updateListeners();" },
                { text: "RefreshX1000",  onclick: "openmdao.Util.refreshX(1000);" }
              ]
            },
            { text: "Tools", 
              items: [
                { text: "Addons", onclick: "openmdao.Util.popupWindow('addons','Addons',575,540)"}
              ]
            },
            { text: "Help", 
              items: [
                { text: "Documentation", onclick: "openmdao.Util.popupWindow('http://openmdao.org/docs');"}
              ]
            },
            { text: "About",             onclick: "openmdao.Util.popupWindow('http://openmdao.org/');"}
        ]

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
