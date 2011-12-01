
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.Console = function(formID,commandID,historyID,model) {    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        command = jQuery('#'+commandID),
        history = jQuery('#'+historyID),
        historyBox = history.parent(),
        contextMenu = jQuery("<ul id="+historyID+"-menu class='context-menu'>"),
        interval = 0,  // ms
        timer = null                   

    // create context menu for history    
    contextMenu.append(jQuery('<li>Clear</li>').click(function(ev) {
        history.html('');
    }));
    contextMenu.append(jQuery('<li>Copy</li>').click(function(ev) {
        openmdao.Util.htmlWindow(history.html());
    }));
    contextMenu.append(jQuery('<li>Update</li>').click(function(ev) {
        update();
    }));
    contextMenu.append(jQuery('<li>Polling...</li>').click(function(ev) {
        promptForRefresh();
    }));
    historyBox.append(contextMenu)
    ContextMenu.set(contextMenu.attr('id'), historyBox.attr('id'));

    // submit a command
    jQuery('#'+formID).submit(function() {
        var cmd = command.val();
        model.issueCommand(command.val(),
            // success, record any response in the history & clear the command
            function(responseText) {
                if (responseText.length > 0) {
                    updateHistory(responseText);
                }
            },
            // failure
            function(jqXHR, textStatus, errorThrown) {
                alert("Error issuing command: "+jqXHR.statusText)
            }
        );
        command.val("");
        return false;
    })
    
    // if an interval is specified, continuously update
    if (interval > 0) {
        setRefresh(interval)
    }

    /** set the history to continuously update after specified ms */
    function setRefresh(interval) {
        self.interval = interval;
        if (timer != 'undefined') {
            clearInterval(timer);
        }
        if (interval > 0) {
            timer = setInterval(update,interval);
        }
    }
    
    /** prompt user for refresh rate */
    promptForRefresh = function() {
        openmdao.Util.promptForValue('Specify a polling delay (in seconds)',function(val) {
            if (val === '0') {
                setRefresh(0);
            }
            else {
                var rate = parseInt(val);
                if (! isNaN(rate)) {
                    setRefresh(rate*1000);
                }
                else {
                    alert('Invalid polling rate, polling is disabled.');
                    setRefresh(0);
                }
            }
        })
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
            } else if(text.charAt(i)== " "){
                result = result + "&nbsp;";
            } else {
                result = result + text.charAt(i);
            }
        }
        return result
    };

    /** scroll to bottom */
    function scrollToBottom() {
        var h = history.height(),
            hb = historyBox.height(),
            hidden = h-hb
        historyBox.scrollTop(hidden);
    }
    
    /** update the history */
    function updateHistory(text) {
        if (text.length > 0) {
            history.append(escapeHTML(text).replace(/\n\r?/g, '<br />'))
            scrollToBottom();
        }
    }
    
    /** update the history with any new output from the model */
    function update() {
        model.getOutput(updateHistory)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)
 
}
