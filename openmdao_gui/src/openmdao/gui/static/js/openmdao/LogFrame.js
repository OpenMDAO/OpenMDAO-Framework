
var openmdao = (typeof openmdao === "undefined" || !openmdao) ? {} : openmdao ;

openmdao.LogFrame = function(id, project) {
    openmdao.LogFrame.prototype.init.call(this, id, 'OpenMDAO Log');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // Note: there is CSS that maps to these specific IDs.
    var self = this,
        current = false,
        paused = false,
        logData = jQuery('<div id="logdata">').appendTo(this.elm),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
                      .appendTo(this.elm),
        pauseButton = jQuery('<li>Pause</li>');

    this.filterCriteria = null;  // List of log levels to accept.

    // Filter a line.
    function filterLine(text) {
        var level = text.charAt(16);
        return self.filterCriteria.indexOf(level) >= 0;
    }

    // Add line(s) to the display.
    function addLine(text) {
        logData.append(openmdao.Util.escapeHTML(text)
                       .replace(/\n\r?/g, '<br />'));
    }

    // Scroll to new bottom.
    function scrollToBottom() {
        var h = logData.height(),
            hb = self.elm.height(),
            hidden = h - hb;
        if (hidden > 0) {
            self.elm.scrollTop(hidden);
        }
    }

    // Pause/resume scrolling.
    function doPause(ev) {
        if (paused) {
            paused = false;
            pauseButton.text('Pause');
            scrollToBottom();
        }
        else {
            paused = true;
            pauseButton.text('Resume');
        }
    }

    // Open log in separate window.
    function doPopout(ev) {
        var init_fn = 'jQuery(function(){openmdao.PopoutLogFrame()})';
        openmdao.Util.scriptWindow('', init_fn);
        self.close();
    }

    // Display filtering options and possibly execute.
    function doFilter(ev) {
        var baseId = 'logfilter',
            levels = ['critical', 'error', 'warning', 'info', 'debug'],
            win = null;

        function handleResponse(ok) {
            if (ok) {
                // Update filter criteria.
                self.filterCriteria = [];
                var allChecked = true;
                jQuery.each(levels, function(idx, level) {
                    var id = baseId+'-'+level,
                        element = jQuery('#'+id);
                    if (element.attr('checked')) {
                        self.filterCriteria.push(level.charAt(0).toUpperCase());
                    } else {
                        allChecked = false;
                    }
                });
                if (allChecked) {
                    self.filterCriteria = null;  // Avoid some overhead.
                }

                // Filter log by completely reloading.
                project.removeListener('log_msgs', updateLog);
                logData.html('');
                current = false;
                project.addListener('log_msgs', updateLog);
            }
            win.dialog('close');
            win.remove();
        }

        win = jQuery('<div id="'+baseId+'"></div>');
        jQuery.each(levels, function(idx, level) {
            var id = baseId+'-'+level,
                enabled = false,
                element;
            if (!self.filterCriteria ||
                self.filterCriteria.indexOf(level.charAt(0).toUpperCase()) >= 0) {
                enabled = true;
            }
            element = jQuery('<input type="checkbox" id="'+id +'" />'
                             +'<label for="'+id+'">Display '+level+' messages</label><br />');
            element.attr('checked', enabled);
            element.appendTo(win);
        });

        win.dialog({
            autoOpen: false,
            modal: true,
            title: 'Log Filtering',
            buttons: [
                {
                    text: 'Ok',
                    id: baseId+'-ok',
                    click: function() { handleResponse(true); }
                },
                {
                    text: 'Cancel',
                    id: baseId+'-cancel',
                    click: function() { handleResponse(false); }
                }
            ]
        });

        win.dialog('open');
    }

    // Copy selection to a plain window.
    // We've take over the browser context menu, and manipulating the
    // clipboard directly is a security no-no.
    function doCopy(ev) {
        // Tried to copy just the selected text, but the selection gets
        // cleared when the context menu is displayed :-(
        openmdao.Util.htmlWindow(logData.html(), 'Copy of OpenMDAO log');
    }

    // Clear display.
    function doClear(ev) {
        logData.html('');
    }

    // Create context menu.
    contextMenu.append(pauseButton.click(doPause));
    if (!this.par) {  // Include if not a pop-out.
        contextMenu.append(jQuery('<li>Pop Out</li>').click(doPopout));
    }
    contextMenu.append(jQuery('<li>Filter</li>').click(doFilter));
    contextMenu.append(jQuery('<li>Copy</li>').click(doCopy));
    contextMenu.append(jQuery('<li><hr /></li>'));
    contextMenu.append(jQuery('<li>Clear</li>').click(doClear));
    ContextMenu.set(contextMenu.attr('id'), this.elm.attr('id'));

    // Update the log.
    function updateLog(msg) {
        var active = msg[1].active,
            text = msg[1].text,
            process = false;
        if (current && active) {
            process = true;
        }
        else if (!current && !active) {
            process = true;
        }
        // Only process messages we need.
        if (process) {
            if (text.length > 0) {
                // Optional filtering.
                if (self.filterCriteria) {
                    if (active) {
                        if (filterLine(text)) {
                            addLine(text);
                        } else {
                            return;
                        }
                    }
                    else {
                        // Split into lines and filter each.
                        var lines = text.split('\n');
                        for (var i = 0 ; i < lines.length ; ++i) {
                            if (filterLine(lines[i])) {
                                addLine(lines[i]);
                                logData.append('<br />');
                            }
                        }
                    }
                }
                else {
                    addLine(text);
                }
                // Keep within window (if not null -- it's happened).
                if (window) {
                    var maxHeight = window.innerHeight * 0.8;
                    if (self.elm.height() > maxHeight) {
                        self.elm.height(maxHeight);
                    }
                    var maxWidth = window.innerWidth * 0.8;
                    if (self.elm.width() > maxWidth) {
                        self.elm.width(maxWidth);
                    }
                }
                // Scroll to new bottom.
                if (!paused) {
                    scrollToBottom();
                }
            }
            else if (!active) {
                // We're now caught up.
                current = true;
            }
        }
    }

    // Ask project for an update whenever a log message is written.
    project.addListener('log_msgs', updateLog);

    this.destructor = function() {
        project.removeListener('log_msgs', updateLog);
    };
};

/** set prototype */
openmdao.LogFrame.prototype = new openmdao.BaseFrame();
openmdao.LogFrame.prototype.constructor = openmdao.LogFrame;

/** initialize a log viewer in a child window */
openmdao.DisplayLogFrame = function() {
    frame = new openmdao.LogFrame('logframe', openmdao.project);
};

/** initialize a log viewer in a separate window */
openmdao.PopoutLogFrame = function() {
    openmdao.project = opener.openmdao.project;
    openmdao.project.addWindow(window);
    jQuery('body').attr('id', 'logbody');
    frame = new openmdao.LogFrame('logbody', openmdao.project);
    window.document.title = 'OpenMDAO Log';
};

