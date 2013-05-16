
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ObjectFrame = function(model, pathname, selectTabName) {
    var divID = 'ObjectFrame_'+pathname.replace(/(\.|\[|\]|\'|\")/g,'-');

    // if there's an existing frame with this id, then just bring it to the front
    if (openmdao.frames.hasOwnProperty(divID)) {
        openmdao.frames[divID].moveToTop();
        return;
    }

    openmdao.ObjectFrame.prototype.init.call(this, divID, 'Object: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        tabbedPane = jQuery('<div id="'+self.id+'_tabs">')
            .appendTo(self.elm),
        tabs = jQuery('<ul>')
            .appendTo(tabbedPane),
        tabOrder = [
            'Inputs',
            'Outputs',
            'Parameters',
            'Objectives',
            'Constraints',
            'CouplingVars',
            'Triggers',
            'Events',
            'Dataflow',
            'Workflow',
            'Slots'
        ],
        panes = {},
        tabcount = 0,
        selected = 0;

    self.elm.css({'overflow':'hidden'});

    selectTabName = selectTabName || 'Inputs';

    /** load the table with the given properties */
    function loadTabs(properties) {
        if (!properties || properties.length === 0) {
            alert('No properties found for ', pathname);
            return;
        }

        // sort the properties by the desired tab order
        var name,
            names = [];
        for (name in properties) {
            if (properties.hasOwnProperty(name)) {
                names.push(name);
            }
        }
        names.sort(function(a, b){
            tab_a = tabOrder.indexOf(a);
            tab_b = tabOrder.indexOf(b);
            return (tab_a == tab_b) ? 0 : (tab_a > tab_b) ? 1 : -1;
        });

        for (var i=0; i<names.length; i++) {
            name = names[i];

            var val = properties[name];

            if (name === 'type') {
                self.setTitle(val+': '+pathname);
            }
            // Don't show empty slots tab.
            else if (name !== 'Slots' || val.length) {
                if (name.length > 12) {
                    tabname = name.substr(0,12);
                }
                else {
                    tabname = name;
                }

                var contentID = self.id+'_'+name,
                    tab = jQuery('<li id="'+contentID+'_tab">')
                        .append('<a href="#'+contentID+'">'+tabname+'</a>'),
                    contentPane = jQuery('<div id="'+contentID+'" style="overflow:auto"></div>');

                tabs.append(tab);
                tabbedPane.append(contentPane);
                getContent(contentPane, name, val);
                if (selectTabName === name) {
                    selected = tabcount;
                }
                tabcount = tabcount + 1;
            }
        }

        self.elm.height(400);
        self.elm.width(640);

        self.elm.tabs({selected: selected})
            .on('tabsshow', function(event, ui) {
                if (ui.tab.text === 'Workflow') {
                    self.elm.find('.WorkflowFigure').trigger('setBackground');
                }
            });

        if (typeof openmdao_test_mode !== 'undefined') {
            openmdao.Util.notify(pathname+' loaded');
        }
    }

    /** populate content pane appropriately for the content */
    function getContent(contentPane, name, val) {
        // TODO: get content pane type more dynamically (a look up table maybe?)
        if (name === 'Inputs') {
            panes[name] = new openmdao.PropertiesPane(contentPane, model,
                                pathname, name, true, true);
            panes[name].loadData(val);
        }
        else if (name === 'Outputs') {
            panes[name] = new openmdao.PropertiesPane(contentPane, model,
                                pathname, name, false, true);
            panes[name].loadData(val);
        }
        else if (name === 'CouplingVars') {
            panes[name] = new openmdao.CouplingVarsPane(contentPane, model,
                                pathname, name, true);
            panes[name].loadData(val);
        }
        else if (name === 'Objectives') {
            panes[name] = new openmdao.ObjectivesPane(contentPane, model,
                                pathname, name, true);
            panes[name].loadData(val);
        }
        else if (name === 'Parameters') {
            panes[name] = new openmdao.ParametersPane(contentPane, model,
                                pathname, name, true);
            panes[name].loadData(val);
        }
        else if (name === 'Constraints') {
            panes[name] = new openmdao.ConstraintsPane(contentPane, model,
                                pathname, name, true);
            panes[name].loadData(val);
        }
        else if (name === 'Workflow') {
            panes[name] = new openmdao.WorkflowPane(contentPane, model,
                                pathname, name);
            panes[name].loadData(val);
        }
        else if (name === 'Dataflow') {
            panes[name] = new openmdao.DataflowPane(contentPane ,model,
                                pathname, name);
            panes[name].loadData(val);
        }
        else if (name === 'Slots') {
            panes[name] = new openmdao.SlotsPane(contentPane, model,
                                pathname, name, false);
            panes[name].loadData(val);
        }
        else if (name === 'Triggers') {
            panes[name] = new openmdao.TriggersPane(contentPane, model,
                                                    pathname, name);
            panes[name].loadData(val);
        }
        else if (name === 'Events') {
            panes[name] = new openmdao.EventsPane(contentPane, model,
                                                  pathname, name);
            panes[name].loadData(val);
        }
        else {
            debug.warn("ObjectFrame.getContent: Unexpected object",
                       pathname, name);
        }
    }

    function loadData(data) {
        var n = 0;
        if (typeof data === 'ArrayBuffer' || data instanceof ArrayBuffer) {
            return;    // FIXME: hack to ignore ArrayBuffer (binary) messages
        }
        jQuery.each(data, function(name, props) {
            ++n;
            if (panes[name]) {
                panes[name].loadData(props);
            }
            else if (name !== 'type' && props) {

                debug.warn("ObjectFrame.loadData: Unexpected interface",
                           pathname, name, props);
            }
        });
        if (n == 0) {  // If no data assume we've been removed.
            self.close();
        }
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== pathname) {
            debug.warn('Invalid object data for:', pathname, message);
            debug.warn('message length', message.length, 'topic', message[0]);
        }
        else {
            loadData(message[1]);
        }
    }

    model.getObject(pathname, loadTabs, function(jqXHR, textStatus, error) {
        debug.warn('ObjectFrame.editObject() Error:', jqXHR, textStatus, error);
        // assume component has been deleted, so close frame
        self.close();
    });

    model.addListener(pathname, handleMessage);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get the specified object from model, load properties into tabs */
    this.update = function() {
        model.getObject(pathname, loadData, function(jqXHR, textStatus, error) {
            debug.warn('ObjectFrame.editObject() Error:', jqXHR, textStatus, error);
            // assume component has been deleted, so close frame
            self.close();
        });
    };

    this.destructor = function() {
        for (var paneName in panes){
            if((panes[paneName].hasOwnProperty('destructor')) &&
                (typeof panes[paneName].destructor === 'function')){
                panes[paneName].destructor();
            }
        }

        if (pathname && pathname.length>0) {
            model.removeListener(pathname, handleMessage);
        }
    };

};

/** set prototype */
openmdao.ObjectFrame.prototype = new openmdao.BaseFrame();
openmdao.ObjectFrame.prototype.constructor = openmdao.ObjectFrame;

