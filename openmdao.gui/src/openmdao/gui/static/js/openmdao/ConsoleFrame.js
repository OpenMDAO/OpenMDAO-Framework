
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConsoleFrame = function(id,model) {
    openmdao.ConsoleFrame.prototype.init.call(this,id,'Console');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // note: there is CSS that maps to these specific IDs, so don't change them :/
    var self = this,
        historyBox = jQuery('<div id="historybox">').appendTo(this.elm),
        history    = jQuery('<div id="history">').appendTo(historyBox),
        cmdform    = jQuery('<form id="cmdform" nostyle="display:none;"  method="post">'
                          + '  <input type="text" id="cmdline" />'
                          + '  <input type="submit" value="Submit" class="button" id="command-button"/>'
                          + '</form>')
            .appendTo(this.elm),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(historyBox);

    var cmdline = cmdform.find('#cmdline'),
        num_commands = 0,
        i_command_current = 0;

    // Need to capture keystrokes on the element with id of "cmdline"
    cmdline.keydown(function(e){
        if (e.keyCode === 38) { // up arrow
            if (i_command_current > 1) {
                i_command_current -= 1;
                cmd = localStorage.getItem("cmd" + i_command_current);
                cmdline.val(cmd);
                if (i_command_current < 1) {
                    i_command_current = 1;
                }
            }
            return false;
        }
        if (e.keyCode === 40) { // down arrow
            if (i_command_current < num_commands ) {
                i_command_current += 1;
                cmd = localStorage.getItem("cmd" + i_command_current);
                cmdline.val(cmd);
            }
            return false;
        }
    });

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
        var init_fn = "jQuery(function(){openmdao.PopoutConsoleFrame()})";
        openmdao.Util.popupScript('Console',init_fn);
    }));
    ContextMenu.set(contextMenu.attr('id'), historyBox.attr('id'));

    // submit a command
    cmdform.submit(function() {
        var cmd = cmdline.val();
        if (cmd.length > 0) {
            cmdline.val("");
            updateHistory('\n>>> '+cmd+'\n');

            num_commands += 1;
            i_command_current = num_commands + 1;
            localStorage.setItem("cmd" + num_commands, cmd);

            model.issueCommand(cmd,
                // success, record any response in history & clear the command
                function(responseText) {
                    if (responseText.length > 0) {
                        updateHistory(responseText);
                    }
                },
                // failure
                function(jqXHR, textStatus, errorThrown) {
                    alert('Error issuing command: '+jqXHR.statusText);
                },
                // completion
                function(jqXHR, textStatus) {
                    if (typeof openmdao_test_mode !== 'undefined') {
                        openmdao.Util.notify("'"+cmd+"' complete: "
                                             +textStatus, 'Console', 'command');
                    }
                }
            );
        }
        return false;
    });

    /** scroll to bottom */
    function scrollToBottom() {
        var h = history.height(),
            hb = historyBox.height(),
            hidden = h-hb;
        historyBox.scrollTop(hidden);
    }

    /** update the history */
    function updateHistory(text) {
        if (text.length > 0) {
            history.append(openmdao.Util.escapeHTML(text).
                            replace(/\n\r?/g, '<br />'));
            scrollToBottom();
        }
    }

    /** display console error */
    function consoleError(msg) {
        text = msg[1];
        updateHistory(text+'\n');
    }

    // ask model for an update whenever something changes
    model.addListener('outstream', updateHistory);

    // ask model for an update whenever a console error occurs.
    model.addListener('console_errors', consoleError);
};

/** set prototype */
openmdao.ConsoleFrame.prototype = new openmdao.BaseFrame();
openmdao.ConsoleFrame.prototype.constructor = openmdao.ConsoleFrame;

/** initialize a console in a child window */
openmdao.PopoutConsoleFrame = function() {
    openmdao.model = opener.openmdao.model;
    openmdao.model.addWindow(window);
    jQuery('body').append('<div id="console"></div>');
    frame = new openmdao.ConsoleFrame("console",  openmdao.model);
    window.document.title='OpenMDAO Console';
};

