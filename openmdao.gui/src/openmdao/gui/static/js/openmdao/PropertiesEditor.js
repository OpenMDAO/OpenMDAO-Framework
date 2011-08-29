
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.PropertiesEditor = function(id,model) {
    openmdao.PropertiesEditor.prototype.init.call(this,id,'Properties');
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this,
        pathname,
        inputs,
        outputs,
        columns = [
            {id:"name",  name:"Name",  field:"name"},
            {id:"value", name:"Value", field:"value", editor:TextCellEditor},
        ],
        inputs_options = {
            editable: true,
            asyncEditorLoading: false,
            editOnDoubleClick: true,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
            //enableAddRow: true,
        },
        outputs_options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        },
        nameHeader = jQuery("<h3>"),
        inputsHeader = jQuery("<h3>Inputs</h3>"),
        outputsHeader = jQuery("<h3>Outputs</h3>"),
        inputsDiv = jQuery("<div id='inputs'>"),
        outputsDiv = jQuery("<div id='outputs'>")

    this.elm.append(nameHeader);
    this.elm.append('<p>');
    this.elm.append(inputsHeader);
    this.elm.append(inputsDiv);
    this.elm.append('<p>');
    this.elm.append(outputsHeader);
    this.elm.append(outputsDiv);

    inputs = new Slick.Grid(inputsDiv, [], columns, inputs_options)
    inputsHeader.click(function () {
        inputsDiv.toggle("normal")
        return false;
    });
    inputs.onCellChange.subscribe(function(e,args) {
        // TODO: better way to do this (e.g. model.setProperty(path,name,value)
        cmd = 'top.'+self.pathname+'.'+args.item.name+'='+args.item.value
        model.issueCommand(cmd)
    });
    
    outputs = new Slick.Grid(outputsDiv, [], columns, outputs_options)       
    outputsHeader.click(function () {
        outputsDiv.toggle("normal")
        return false;
    });
    
    model.addListener(update);
          
    /** load the table with the given properties */
    function loadTables(properties) {
        if (properties['type']) {
            nameHeader.html(properties['type']+': '+self.pathname)
            inputs.setData(properties['Inputs'])
            outputs.setData(properties['Outputs'])
        }
        else {
            nameHeader.html(self.pathname)
            inputs.setData([])
            outputs.setData([])
            alert('Error getting properties for '+self.pathname)
            debug.info(properties)
        }
        inputs.updateRowCount()
        inputs.render()
        outputs.updateRowCount()
        outputs.render()
    }
    
    /** if there is an object loaded, update it from the model */
    function update() {
        if (self.pathname && self.pathname.length>0)
            self.editObject(self.pathname)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** get the specified object from model, load properties into table */
    this.editObject = function(path) {
        if (self.pathname !== path)
            self.pathname = path
        model.getComponent(path, loadTables,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error getting properties for "+self.pathname+" (status="+jqXHR.status+"): "+jqXHR.statusText)
                openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting properties',600,400)
                debug.error(jqXHR,textStatus,errorThrown)
            }
        )
        return this
    }
    
}

/** set prototype */
openmdao.PropertiesEditor.prototype = new openmdao.BasePane();
openmdao.PropertiesEditor.prototype.constructor = openmdao.PropertiesEditor;