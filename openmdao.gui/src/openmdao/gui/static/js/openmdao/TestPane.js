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
    this.prototype = new openmdao.BasePane()
    
    /***********************************************************************
     *  private
     ***********************************************************************/
        
    var self = this,
        title    = "Test Pane",
        helpHTML = "<div>"+
                   "This is just a test of a basic pane which can be built upon... <br/>"+
                   "</div>",
        menu     = [
                     { "text": "Menu", 
                       "items": [
                         { "text": "Test",  "onclick": "alert('Test complete!');" },
                       ]
                     },
                     { "text": "Help",      "onclick": "jQuery('"+helpHTML+"').dialog({'title':'"+title+"','width':400,'height':150})" }
                   ]

    function init(id,model) {
        self.prototype.init(id,title,menu)
        // build your ui, access openmdao via model
        // ...
    }
    
    if (arguments.length > 0) {
        init(id,model)
    }

}



