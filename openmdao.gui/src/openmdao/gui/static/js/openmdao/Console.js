
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
        sck = null

    // create context menu for history    
    contextMenu.append(jQuery('<li>Trace</li>').click(function(ev) {
        model.issueCommand('trace');
    }));
    contextMenu.append(jQuery('<li>Clear</li>').click(function(ev) {
        history.html('');
    }));
    contextMenu.append(jQuery('<li>Copy</li>').click(function(ev) {
        openmdao.Util.htmlWindow(history.html());
    }));
    historyBox.append(contextMenu)
    ContextMenu.set(contextMenu.attr('id'), historyBox.attr('id'));

    // submit a command
    jQuery('#'+formID).submit(function() {
        var cmd = command.val();
        if (cmd.length > 0) {
            command.val("");
            updateHistory('\n>>> '+cmd+'\n');
            model.issueCommand(cmd,
                // success, record any response in the history & clear the command
                function(responseText) {
                    if (responseText.length > 0) {
                        updateHistory(responseText);
                    }
                },
                // failure
                function(jqXHR, textStatus, errorThrown) {
                    alert('Error issuing command: '+jqXHR.statusText)
                }
            );
        }
        return false;
    })
    
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
            history.append(openmdao.Util.escapeHTML(text).replace(/\n\r?/g, '<br />'))
            scrollToBottom();
        }
    }

    // ask model for an update whenever something changes
    model.addListener('outstream',updateHistory)

}
