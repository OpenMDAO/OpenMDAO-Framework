
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.TestFrame = function(id,model) {
    this.prototype = new openmdao.BaseFrame()
    
    /***********************************************************************
     *  private
     ***********************************************************************/
        
    var self = this,
        title    = "Test Pane",
        helpHTML = "<div>"+
                   "This is just a test of a basic frame which can be built upon... <br/>"+
                   "</div>",
        menu     = [
                     { "text": "Menu", 
                       "items": [
                         { "text": "Test",  "onclick": "alert('Test complete!');" },
                       ]
                     },
                     { "text": "Help",      "onclick": "jQuery('"+helpHTML+"').dialog({'title':'"+title+"','width':400,'height':150})" }
                   ];

    openmdao.TestFrame.prototype.init.call(this,id,title,menu);
    
    // build your ui, access openmdao via model, etc.
    // ...
    
}

/** set prototype */
openmdao.TestFrame.prototype = new openmdao.BaseFrame();
openmdao.TestFrame.prototype.constructor = openmdao.TestFrame;