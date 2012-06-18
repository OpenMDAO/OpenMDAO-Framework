
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConnectionsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('ConnectionsFrame-'+pathname).replace(/\./g,'-');
    openmdao.ConnectionsFrame.prototype.init.call(this, id,
        'Connections: '+openmdao.Util.getName(pathname));

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        components_css = 'overflow:none; background:grey;',
        components_div = jQuery('<div id='+id+' style="'+components_css+'">')
            .appendTo(self.elm),
        html = '<table cellpadding="0" cellspacing="0" border="0">'
             +        '<tr><td>Source Component:</td><td>Target Component:</td></tr>'
             +        '<tr><td><input id="src_list" class="combobox" /></td>'
             +        '    <td><input id="dst_list" class="combobox" /></td></tr>'
             + '</table>',
        components = jQuery(html)
            .appendTo(components_div),
        src_selector = components_div.find('#src_list'),
        dst_selector = components_div.find('#dst_list'),
        connections_css = 'height:auto;width:auto;background:grey;position:absolute',
        connections_div = jQuery('<div style="'+connections_css+'">')
            .appendTo(components_div),
        connections_pane = null;

    self.pathname = null;
    self.src_comp = src_comp;
    self.dst_comp = dst_comp;

    function editConnections(pathname, src_comp, dst_comp) {
        if (src_comp && dst_comp) {
            if (connections_pane === null) {
                connections_pane = new openmdao.ConnectionsPane(connections_div,
                                           model, pathname, src_comp, dst_comp);
                connections_div.append(connections_pane);
            }
            else {
                connections_pane.editConnections(pathname, src_comp, dst_comp);
            }
        }
        else {
            // remove connections pane if src & dest components are not selected
            if (connections_pane !== null) {
                connections_pane = null;
                connections_div.html('');
            }
        }
    }

    function bindEnterKey(selector) {
        selector.autocomplete({
           select: function(event, ui) {
               selector.value = ui.item.value;
               ent = jQuery.Event('keypress.enterkey');
               ent.target = selector;
               ent.which = 13;
               selector.trigger(ent);
           },
           delay: 0,
           minLength: 0
        });
        selector.bind('keypress.enterkey', function(e) {
            if (e.which === 13) {
                selector.autocomplete('close');
                if (selector === src_selector) {
                    self.src_comp = e.target.value;
                }
                else {
                    self.dst_comp = e.target.value;
                }
                editConnections(self.pathname, self.src_comp, self.dst_comp);
            }
        });
    }

    bindEnterKey(src_selector);
    bindEnterKey(dst_selector);

    function loadData(data) {
        if (!data || !data.Dataflow || !data.Dataflow.components) {
            // don't have what we need, probably something got deleted
            self.close();
        }
        else {
            comp_list = jQuery.map(data.Dataflow.components,
                                   function(comp,idx){ return comp.name; });

            // update the output & input selectors with component list
            src_selector.html('');
            src_selector.autocomplete({source: comp_list});

            dst_selector.html('');
            dst_selector.autocomplete({source: comp_list});

            //jQuery.each(comp_list,function (idx,name) {
            //    src_selector.append('<option value="'+name+'">'+name+'</option>');
            //});
            //jQuery.each(comp_list,function (idx,name) {
            //    dst_selector.append('<option value="'+name+'">'+name+'</option>');
            //});
        }

        if (self.src_comp) {
            src_selector.val(self.src_comp);
        }
        if (self.dst_comp) {
            dst_selector.val(self.dst_comp);
        }
        editConnections(self.pathname, self.src_comp, self.dst_comp);
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
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
        if (self.pathname !== path) {
           if (self.pathname !== null) {
                model.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            model.addListener(self.pathname, handleMessage);
        }

        model.getComponent(path, loadData,
            function(jqXHR, textStatus, errorThrown) {
                debug.warn('ConnectionsFrame.editObject() Error:',
                            jqXHR, textStatus, errorThrown);
                // assume component has been deleted, so close frame
                self.close();
            }
        );
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

    this.editObject(pathname);
};

/** set prototype */
openmdao.ConnectionsFrame.prototype = new openmdao.BaseFrame();
openmdao.ConnectionsFrame.prototype.constructor = openmdao.ConnectionsFrame;
