
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.Console = function(id,model) {  
    openmdao.Console.prototype.init.call(this,id,'Console');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // note: there is CSS that maps to these specific IDs, so don't change them :/
    var self = this,
        historyBox = jQuery('<div id="historybox">').appendTo(this.elm),
        history    = jQuery('<div id="history">').appendTo(historyBox),
        cmdform    = jQuery('<form id="cmdform" nostyle="display:none;"  method="post">'
                          + '  <input type="text" id="command" />'
                          + '  <input type="submit" value="Submit" class="button" id="command-button"/>'
                          + '</form>').appendTo(this.elm),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>").appendTo(historyBox);

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
    contextMenu.append(jQuery('<li>Pop Out</li>').click(function(ev) {
        var init_fn = "jQuery(function(){openmdao.PopoutConsole()})";
        openmdao.Util.popupScript('Console',init_fn);
    }));
    ContextMenu.set(contextMenu.attr('id'), historyBox.attr('id'));

    // submit a command
    cmdform.submit(function() {
        var command = cmdform.children('#command');
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
                    alert('Error issuing command: '+jqXHR.statusText);
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

/** set prototype */
openmdao.Console.prototype = new openmdao.BaseFrame();
openmdao.Console.prototype.constructor = openmdao.Console;

/** initialize a console in a child window */
openmdao.PopoutConsole = function() {
	openmdao.model = opener.openmdao.model;
    jQuery('body').append('<div id="console"></div>');
	new openmdao.Console("console",  openmdao.model) 
}
