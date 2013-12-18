
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConsoleFrame = function(id, project) {
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
    contextMenu.append(jQuery('<li title="Show stack trace for last error">Trace</li>').click(function(ev) {
        project.issueCommand('trace');
    }));
    contextMenu.append(jQuery('<li title="Clear console history">Clear</li>').click(function(ev) {
        history.html('');
    }));
    contextMenu.append(jQuery('<li title="Open history in new window">Copy</li>').click(function(ev) {
        openmdao.Util.htmlWindow(history.html());
    }));
    contextMenu.append(jQuery('<li title="Open a pop-out console window">Pop Out</li>').click(function(ev) {
        openmdao.Util.scriptWindow('Console', 'jQuery(function(){openmdao.PopoutConsoleFrame()})');
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

            project.issueCommand(cmd)
                .done(function(responseText) {
                    if (responseText.length > 0) {
                        updateHistory(responseText);
                    }
                })
                .fail(function(jqXHR, textStatus, errorThrown) {
                    alert('Error issuing command: '+jqXHR.statusText);
                })
                .always(function(jqXHR, textStatus) {
                    if (typeof openmdao_test_mode !== 'undefined') {
                        openmdao.Util.notify("'"+cmd+"' complete: "
                                             +textStatus, 'Console', 'command');
                    }
                });
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

    // ask project for an update whenever something changes
    project.addListener('outstream', updateHistory);

    // ask project for an update whenever a console error occurs.
    project.addListener('console_errors', consoleError);
};

/** set prototype */
openmdao.ConsoleFrame.prototype = new openmdao.BaseFrame();
openmdao.ConsoleFrame.prototype.constructor = openmdao.ConsoleFrame;

/** initialize a console in a child window */
openmdao.PopoutConsoleFrame = function() {
    openmdao.project = opener.openmdao.project;
    openmdao.project.addWindow(window);
    jQuery('body').append('<div id="console"></div>');
    new openmdao.ConsoleFrame("console",  openmdao.project);
    window.document.title='OpenMDAO Console';
};

