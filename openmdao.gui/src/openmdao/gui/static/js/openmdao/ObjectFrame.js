
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ObjectFrame = function(model,pathname,tabName) {
    // TODO: hack alert... mangling pathname
    openmdao.ObjectFrame.prototype.init.call(this,
        'CE-'+pathname.replace(/(\.|\[|\])/g,'-'),'Object: '+pathname);

    this.initiallySelected = tabName || 'Inputs';

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        panes = {},
        tab_order = [
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
        ];

    self.elm.css({'overflow':'hidden'});

    /** load the table with the given properties */
    function loadTabs(properties) {
        if (!properties || properties.length === 0) {
            alert('No properties found for ',self.pathname);
            return;
        }

        var tabbed_pane = jQuery('<div id="'+self.id+'_tabs">'),
            tabs = jQuery('<ul>'),
            tabcount = 0,
            selected = 0;

        self.elm.html("");
        self.elm.append(tabbed_pane);
        tabbed_pane.append(tabs);

        // sort the properties by the desired tab order
        var names = [];
        for (var name in properties) {
            if (properties.hasOwnProperty(name)) {
                names.push(name);
            }
        }
        names.sort(function(a, b){
            tab_a = tab_order.indexOf(a);
            tab_b = tab_order.indexOf(b);
            return (tab_a == tab_b) ? 0 : (tab_a > tab_b) ? 1 : -1;
        })

        for (var i=0; i<names.length; i++) {
            var name = names[i],
                val = properties[name];

            if (name === 'type') {
                if (self.elm.parent().hasClass('ui-dialog')) {
                    self.elm.dialog("option","title",val+': '+self.pathname);
                }
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
                tabbed_pane.append(contentPane);
                getContent(contentPane,name,val);
                if (self.initiallySelected === name) {
                    selected = tabcount;
                }
                tabcount = tabcount + 1;
            }
        };

        self.elm.height(400);
        self.elm.width(640);

        self.elm.tabs({selected: selected})
            .on('tabsshow', function(event, ui) {
                if (ui.tab.text === 'Workflow') {
                    self.elm.find('.WorkflowFigure').trigger('setBackground');
                }
            });

        if (typeof openmdao_test_mode !== 'undefined') {
            openmdao.Util.notify(self.pathname+' loaded');
        }
    }

    /** populate content pane appropriately for the content */
    function getContent(contentPane, name, val) {
        // TODO: get content pane type more dynamically (a look up table maybe?)
        if (name === 'Inputs') {
            panes[name] = new openmdao.PropertiesPane(contentPane,model,
                                self.pathname,name,true,true);
            panes[name].loadData(val);
        }
        else if (name === 'Outputs') {
            panes[name] = new openmdao.PropertiesPane(contentPane,model,
                                self.pathname,name,false,true);
            panes[name].loadData(val);
        }
        else if (name === 'CouplingVars') {
            panes[name] = new openmdao.CouplingVarsPane(contentPane,model,
                                self.pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name === 'Objectives') {
            panes[name] = new openmdao.ObjectivesPane(contentPane,model,
                                self.pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name === 'Parameters') {
            panes[name] = new openmdao.ParametersPane(contentPane,model,
                                self.pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name === 'Constraints') {
            panes[name] = new openmdao.ConstraintsPane(contentPane,model,
                                self.pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name === 'Workflow') {
            panes[name] = new openmdao.WorkflowPane(contentPane,model,
                                self.pathname,name);
            panes[name].loadData(val);
        }
        else if (name === 'Dataflow') {
            panes[name] = new openmdao.DataflowPane(contentPane,model,
                                self.pathname,name);
            panes[name].loadData(val);
        }
        else if (name === 'Slots') {
            panes[name] = new openmdao.SlotsPane(contentPane,model,
                                self.pathname,name,false);
            panes[name].loadData(val);
        }
        else if (name === 'Triggers') {
            panes[name] = new openmdao.TriggersPane(contentPane, model,
                                                  self.pathname, name);
            panes[name].loadData(val);
        }
        else if (name === 'Events') {
            panes[name] = new openmdao.EventsPane(contentPane, model,
                                                  self.pathname, name);
            panes[name].loadData(val);
        }
        else {
            debug.warn("ObjectFrame.getContent: Unexpected object",
                       self.pathname, name);
        }
    }

    function loadData(ifaces) {
        var nIfaces = 0;
        jQuery.each(ifaces,function (name,props) {
            ++nIfaces;
            if (panes[name]) {
                panes[name].loadData(props);
            }
            else if (name !== 'type' && props) {
                debug.warn("ObjectFrame.loadData: Unexpected interface",
                           self.pathname, name, props);
            }
        });
        if (!nIfaces) {  // If no data assume we've been removed.
            self.close();
        }
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid object data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            loadData(message[1]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** if there is an object loaded, update it from the model */
    this.update = function() {
        // TODO: should just update existing panes rather than recreate them
        if (self.pathname && self.pathname.length>0) {
            self.editObject(self.pathname);
        }
    };

    /** get the specified object from model, load properties into tabs */
    this.editObject = function(path) {
        var callback = loadTabs;
        if (self.pathname !== path) {
           if (self.pathname) {
                model.removeListener(self.pathname, handleMessage);
            }

            self.pathname = path;
            callback = loadTabs;    // recreate tabs

            // listen for messages and update object properties accordingly
            model.addListener(self.pathname, handleMessage);
        }

        model.getObject(path, callback,
            function(jqXHR, textStatus, errorThrown) {
                debug.warn('ObjectFrame.editObject() Error:',
                            jqXHR, textStatus, errorThrown);
                // assume component has been deleted, so close frame
                self.close();
            }
        );
        return this;
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

    this.editObject(pathname);

};

/** set prototype */
openmdao.ObjectFrame.prototype = new openmdao.BaseFrame();
openmdao.ObjectFrame.prototype.constructor = openmdao.ObjectFrame;

