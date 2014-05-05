
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ObjectFrame = function(project, pathname, selectTabName) {
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
    var _self = this,
        _tabbedPane = jQuery('<div id="'+_self.id+'__tabs">')
            .appendTo(_self.elm),
        _tabs = jQuery('<ul>')
            .appendTo(_tabbedPane),
        _tabNames = [
            'Inputs',
            'Outputs',
            'States',
            'Residuals',
            'Parameters',
            'Objectives',
            'Responses',
            'Constraints',
            'CouplingVars',
            'Triggers',
            'Events',
            'Dataflow',
            'Workflow',
            'Slots',
            'Drawing'
        ],
        _panes = {},
        _updates = [];  // queue for updates

    _self.elm.css({'overflow':'hidden'});

    selectTabName = selectTabName || 'Inputs';

    /** create and populate a tabbed pane appropriate to the named interface */
    function createTab(name, val) {
        var contentID = _self.id+'_'+name,
            tabLabel = name.substr(0, 12),
            tab = jQuery('<li id="'+contentID+'_tab">')
                .append('<a href="#'+contentID+'">'+tabLabel+'</a>'),
            contentPane = jQuery('<div id="'+contentID+'" style="overflow:auto"></div>');

        _tabs.append(tab);
        _tabbedPane.append(contentPane);

        // TODO: get content pane type more dynamically (a look up table maybe?)
        if (name === 'Inputs') {
            _panes[name] = new openmdao.PropertiesPane(contentPane, project,
                                pathname, name, true, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Outputs') {
            _panes[name] = new openmdao.PropertiesPane(contentPane, project,
                                pathname, name, false, true);
            _panes[name].loadData(val);
        }
        else if (name === 'States') {
            _panes[name] = new openmdao.PropertiesPane(contentPane, project,
                                pathname, name, true, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Residuals') {
            _panes[name] = new openmdao.PropertiesPane(contentPane, project,
                                pathname, name, false, true);
            _panes[name].loadData(val);
        }
        else if (name === 'CouplingVars') {
            _panes[name] = new openmdao.CouplingVarsPane(contentPane, project,
                                pathname, name, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Objectives') {
            _panes[name] = new openmdao.ObjectivesPane(contentPane, project,
                                pathname, name, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Responses') {
            _panes[name] = new openmdao.ResponsesPane(contentPane, project,
                                pathname, name, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Parameters') {
            _panes[name] = new openmdao.ParametersPane(contentPane, project,
                                pathname, name, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Constraints') {
            _panes[name] = new openmdao.ConstraintsPane(contentPane, project,
                                pathname, name, true);
            _panes[name].loadData(val);
        }
        else if (name === 'Workflow') {
            _panes[name] = new openmdao.WorkflowPane(contentPane, project,
                                pathname, name);
            _panes[name].loadData(val);
        }
        else if (name === 'Dataflow') {
            _panes[name] = new openmdao.DataflowPane(contentPane ,project,
                                pathname, name);
            _panes[name].loadData(val);
        }
        else if (name === 'Slots') {
            _panes[name] = new openmdao.SlotsPane(contentPane, project,
                                pathname, name, false);
            _panes[name].loadData(val);
        }
        else if (name === 'Triggers') {
            _panes[name] = new openmdao.TriggersPane(contentPane, project,
                                                     pathname, name);
            _panes[name].loadData(val);
        }
        else if (name === 'Events') {
            _panes[name] = new openmdao.EventsPane(contentPane, project,
                                                   pathname, name);
            _panes[name].loadData(val);
        }
        else if (name === 'Drawing') {
            _panes[name] = new openmdao.DrawingPane(contentPane, project,
                                                    pathname, name);
            _panes[name].loadData(val);
        }
        else {
            debug.warn("ObjectFrame.createTab: Unexpected interface",
                       pathname, name);
        }
    }

    /** destroy the named pane and remove it from dictionary */
    function removePane(name) {
        if((_panes[name].hasOwnProperty('destructor')) &&
            (typeof _panes[name].destructor === 'function')){
            _panes[name].destructor();
        }
        delete _panes[name];
    }

    /** get list of of interface names sorted in the desired tab order */
    function getSortedNames(properties) {
        var name,
            names = [];
        for (name in properties) {
            if (properties.hasOwnProperty(name)) {
                names.push(name);
            }
        }
        names.sort(function(a, b){
            tab_a = _tabNames.indexOf(a);
            tab_b = _tabNames.indexOf(b);
            return (tab_a == tab_b) ? 0 : (tab_a > tab_b) ? 1 : -1;
        });
        return names;
    }

    /** load object properties into the respective tabbed panes */
    function loadData(properties, deferred) {
        var names, name, val;

        // remove click handlers from existing tabs before updating
        _tabs.find('.ui-tabs-anchor').off('click');

        // FIXME: hack to ignore ArrayBuffer (binary) messages
        if (typeof properties === 'ArrayBuffer' || properties instanceof ArrayBuffer) {
            return;
        }

        if (!properties || (Object.keys(properties).length === 0)) {
            openmdao.Util.notify('No properties found for: ' + pathname,
                                 'No properties found');
            return;
        }
        names = getSortedNames(properties);

        // if object has no interfaces, assume it's been removed
        if (names.length === 0) {
            _self.close();
        }

        // update existing type/tabs, add any new tabs
        for (var i=0; i<names.length; i++) {
            name = names[i];
            val = properties[name];

            if (name === 'type') {
                _self.setTitle(val + ': ' + pathname);
            }
            else if (_panes[name]) {
                _panes[name].loadData(val);
            }
            else if (name !== 'Slots' || val.length) {
                createTab(name, val);
            }
            else {
                debug.warn('ObjectFrame.loadData: Unexpected interface',
                           pathname, name, val);
            }
        }

        // remove defunct tabs and panes
        jQuery.each(_self.elm.find('.ui-tabs-anchor'), function(idx, tab) {
            var tabLI, divID;
            if (! properties.hasOwnProperty(tab.text)) {
                tabLI = jQuery(tab).closest('li');
                divID = tabLI.attr('aria-controls');
                tabLI.remove();
                _self.elm.find('#'+divID).remove();
                removePane(tab.text);
            }
        });

        // refresh
        _self.elm.tabs('refresh');

        // select the appropriate tab
        _self.selectTab(selectTabName);

        // add handler to update selected tab when clicked
        _tabs.find('.ui-tabs-anchor').on('click', function(ev, data) {
            selectTabName = ev.currentTarget.text;
        });

        deferred.resolve();
    }

    function queueUpdate(data) {
        var old_update = _updates.shift(),
            new_update = jQuery.Deferred();

        _updates.push(new_update);

        if (old_update) {
            // make sure old update is done first
            jQuery.when(old_update).done(function() {
                loadData(data, new_update);
            });
        }
        else {
            loadData(data, new_update);
        }
    }

    /** update with data from incoming message */
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== pathname) {
            debug.warn('Invalid object data for:', pathname, message);
            debug.warn('message length', message.length, 'topic', message[0]);
        }
        else {
            if (Object.keys(message[1]).length > 0) {
                queueUpdate(message[1]);
            }
            else {
                _self.close();  // no data means the object was deleted
            }
        }
    }

    /** initialize the frame with the given properties */
    function init(properties) {
        if (!properties || (Object.keys(properties).length === 0)) {
            _self.close();
            openmdao.Util.notify('No properties found for: ' + pathname,
                                 'No properties found');
            return;
        }

        _self.elm.height(400);
        _self.elm.width(720);
        _self.elm.tabs({
            activate: function(event, ui) {
                if (ui.newTab.text() === 'Workflow') {
                    _self.elm.find('.WorkflowFigure').trigger('layoutAll');
                }
            }
        });

        queueUpdate(properties);

        project.addListener(pathname, handleMessage);

        if (typeof openmdao_test_mode !== 'undefined') {
            openmdao.Util.notify(pathname+' loaded');
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get the object properties from project, load data into tabbed panes */
    this.update = function(callback) {
        callback = callback || queueUpdate;
        project.getObject(pathname)
            .done(callback)
            .fail(function(jqXHR, textStatus, error) {
                debug.warn('ObjectFrame.editObject() Error:', jqXHR, textStatus, error);
                // assume component has been deleted, so close frame
                _self.close();
            });
    };

    /** select the tab with the specified label */
    this.selectTab = function(tabName) {
        var tab = _self.elm.find('li:contains("'+tabName+'")'),
            tabID = tab.attr('id'),
            tabIndex = tab.parent().find('#'+tabID).index();
        _self.elm.tabs('option', 'active', tabIndex);
    };

    /** destroy all panes and unsubscribe to messages */
    this.destructor = function() {
        for (var paneName in _panes){
            removePane(paneName);
        }
        if (pathname && pathname.length>0) {
            project.removeListener(pathname, handleMessage);
        }
    };

    // initialize
    _self.update(init);
};

/** set prototype */
openmdao.ObjectFrame.prototype = new openmdao.BaseFrame();
openmdao.ObjectFrame.prototype.constructor = openmdao.ObjectFrame;

