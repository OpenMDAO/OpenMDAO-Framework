/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.BasePane = function() {
    this.elm = null,
    
    this.init = function (id,title,menu) {
        elm = jQuery("#"+id)

        // if the elm doesn't exist, create it as a popup 
        if (elm.length === 0) {
            elm = jQuery('<div id='+id+'></div>')        
            elm.dialog({
                'modal': false,
                'title': title+': '+id,
                'close': function(ev, ui) { elm.remove(); },
                width: 600, 
                height: 400
            })
        }
        else {
            elm.html("")
        }

        // set the title
        if (title)
            elm.attr('title',title)
            
        // delete any existing content and prevent browser context menu
        elm.html("").bind("contextmenu", function(e) { return false; })
        
        // add menu
        if (menu) {
            var menuID = id+"-menu"
            elm.append("<nav2 id='"+menuID+"'>")
            new openmdao.Menu(menuID,menu)
        }
        
        debug.info("BasePane.init",this)
    }
}

