
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.Console = function(formID,commandID,historyID,model) {    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        command = jQuery('#'+commandID),
        history = jQuery('#'+historyID),
        historyBox = history.parent(),
        menuhtml = "<ul id="+historyID+"-menu class='context-menu'>" +
                   "<li><a onclick=jQuery('#"+historyID+"').text('')>Clear</a></li>" +
                   "</ul>",
        interval = 0,  // ms
        timer = null                   

    // create context menu for history
    historyBox.append(menuhtml)
    ContextMenu.set(historyID+"-menu", historyBox.attr('id'));

    /** DEBUG: make the history pane droppable */
    historyBox.droppable({
        accept: '.ui-draggable',
        drop: function(ev,ui) { 
            var droppedObject = jQuery(ui.draggable).clone();
            debug.info('Console drop',droppedObject)
        }
    })    
    
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
    
    // if an interval is specified, continuously update
    if (interval > 0) {
        setRefresh(interval)
    }

    /** set the history to continuously update after specified ms */
    function setRefresh(interval) {
        self.interval = interval
        if (timer != 'undefined')
            clearInterval(timer)
        timer = setInterval(update,interval)    
    }
        
    /** escape anything in the text that might look like HTML, etc. */
    function escapeHTML(text) {
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

    /** update the history */
    function updateHistory(text) {
        if (text.length > 0) {
            history.append(escapeHTML(text).replace(/\n\r?/g, '<br />'))
            var h = history.height(),
                hb = historyBox.height(),
                hidden = h-hb
            historyBox.scrollTop(h-(hidden));
        }
    }
    
    /** update the history with any new output from the model */
    function update() {
        model.getOutput(updateHistory)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)
 
}
