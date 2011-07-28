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
openmdao.TestPane = function(id,model) {
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        elm = jQuery("#"+id),
        title = "Test Pane"
        menuDiv = jQuery("<nav2 id='"+id+"-menu'>"),
        helpHTML = "<div>"+
            "This is just a test of a basic pane which can be built upon... <br/>"+
            "</div>",
        menu = [
            { "text": "Menu", 
              "items": [
                { "text": "Test",          "onclick": "alert('Test complete!');" },
              ]
            },
            { text: "Help", onclick: "jQuery('"+helpHTML+"').dialog({'title':'"+title+"','width':400,'height':150})" }
        ]

    // if the elm doesn't exist, create a popup 
    if (elm.length === 0) {
        elm = jQuery('<div id='+id+'></div>')        
        elm.dialog({
            'modal': false,
            'title': title+': '+id,
            'close': function(ev, ui) { elm.remove(); },
            width: 640, 
            height: 480 
        })
    }
    else {
        elm.html("")
    }
    
    elm.html("").bind("contextmenu", function(e) { return false; })
    elm.append(menuDiv);
    new openmdao.Menu(menuDiv.attr('id'),menu)

    // build your ui, access openmdao via model
    // ...
}

