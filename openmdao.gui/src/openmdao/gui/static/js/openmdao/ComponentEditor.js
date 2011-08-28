
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ComponentEditor = function(model,pathname) {
    openmdao.ComponentEditor.prototype.init.call(this,'_'+pathname,'Component');
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this
        
    model.addListener(update)
          
    /** load the table with the given properties */
    function loadTabs(properties) {
        var paneID = self.id+'_tabs';
        self.elm.html('<dl id="'+paneID+'"></dl>');
        if (properties['inputs']) {
            self.elm.append('<dt id="'+self.id+'_inputs_tab">Inputs</dt>');
            self.elm.append('<dd id="'+self.id+'_inputs_pane"><div id="'+self.id+'_inputs"></div></dd>');
        }
        if (properties['outputs']) {
            self.elm.append('<dt id="'+self.id+'_outputs_tab">Outputs</dt>');
            self.elm.append('<dd id="'+self.id+'_outputs_pane"><div id="'+self.id+'_outputs"></div></dd>');
        }
        if (properties['parameters']) {
            self.elm.append('<dt id="'+self.id+'_parameters_tab">Parameters</dt>');
            self.elm.append('<dd id="'+self.id+'_parameters_pane"><div id="'+self.id+'_parameters"></div></dd>');
        }
        if (properties['objectives']) {
            self.elm.append('<dt id="'+self.id+'_objectives_tab">Objectives</dt>');
            self.elm.append('<dd id="'+self.id+'_parameters_pane"><div id="'+self.id+'_objectives"></div></dd>');
        }
        if (properties['eqconstraints']) {
            self.elm.append('<dt id="'+self.id+'_eqconstraints_tab">EqConstraints</dt>');
            self.elm.append('<dd id="'+self.id+'_parameters_pane"><div id="'+self.id+'_eqconstraints"></div></dd>');
        }
        if (properties['ineqconstraints']) {
            self.elm.append('<dt id="'+self.id+'_ineqconstraints_tab">IneqConstraints</dt>');
            self.elm.append('<dd id="'+self.id+'_parameters_pane"><div id="'+self.id+'_ineqconstraints"></div></dd>');
        }
        openmdao.TabbedPane(self.id);
    }
    
    /** if there is an object loaded, update it from the model */
    function update() {
        if (self.pathname && self.pathname.length>0)
            self.editObject(self.pathname)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** get the specified object from model, load properties into tabs */
    this.editObject = function(path) {
        if (self.pathname !== path)
            self.pathname = path
        model.getComponent(path, loadTabs,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error getting properties for "+self.pathname+" (status="+jqXHR.status+"): "+jqXHR.statusText)
                openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting properties',600,400)
                debug.error(jqXHR,textStatus,errorThrown)
            }
        )
        return this
    }

    if (pathname) {
        this.editObject(pathname)
    }

}

/** set prototype */
openmdao.ComponentEditor.prototype = new openmdao.BasePane();
openmdao.ComponentEditor.prototype.constructor = openmdao.ComponentEditor;