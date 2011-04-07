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
openmdao.Console = function(formID,commandID,historyID,model) {    
    /***********************************************************************
     *  private (available only to privileged methods) 
     ***********************************************************************/
     
    var that = this,
        command = jQuery('#'+commandID),
        history = jQuery('#'+historyID),
        historyBox = history.parent(),
        menuhtml = "<ul id="+historyID+"-menu class='context-menu'>" +
                   "<li><a onclick=jQuery('#"+historyID+"').text('')>Clear</a></li>" +
                   "</ul>"

    // create context menu for history
    historyBox.append(menuhtml)
    ContextMenu.set(historyID+"-menu", historyBox.attr('id'));

    // submit a command
    jQuery('#'+formID).submit(function() {
        model.issueCommand(command.val(),
            // success, record any response in the history & clear the command
            function(responseText) {
                if (responseText.length > 0)
                    updateHistory(responseText)
                command.val("")
            },
            // failure
            function(jqXHR, textStatus, errorThrown) {
                alert("Error issuing command: "+jqXHR.statusText)
            }
        )
        return false
    })
    
    // escape anything in the text that might look like HTML or something
    escapeHTML = function(text) {
        var result = "";
        for(var i = 0; i < text.length; i++){
            if(text.charAt(i) == "&" 
                  && text.length-i-1 >= 4 
                  && text.substr(i, 4) != "&amp;"){
                result = result + "&amp;";
            } else if(text.charAt(i)== "<"){
                result = result + "&lt;";
            } else if(text.charAt(i)== ">"){
                result = result + "&gt;";
            } else {
                result = result + text.charAt(i);
            }
        }
        return result
    };

    // update the history
    updateHistory = function(text) {
        if (text.length > 0) {
            history.append(escapeHTML(text).replace(/\n\r?/g, '<br />'))
            var h = history.height(),
                hb = historyBox.height(),
                hidden = h-hb
            historyBox.scrollTop(h-(hidden));
        }
    }
    
    // update the history with any new output from the model
    function update() {
        model.getOutput(updateHistory)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)
    
    /***********************************************************************
     *  privileged (can access privates, accessible to public and outside) 
     ***********************************************************************/
 
}
