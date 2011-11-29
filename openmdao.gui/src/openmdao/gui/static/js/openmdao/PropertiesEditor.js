
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
    this.elm.width(200);

    inputs = new openmdao.PropertiesPane(inputsDiv,model,self.pathname,'Inputs',true)
    inputsHeader.click(function () {
        inputsDiv.toggle("normal")
        return false;
    });
    
    outputs = new openmdao.PropertiesPane(outputsDiv,model,self.pathname,'Outputs',false)
    outputsHeader.click(function () {
        outputsDiv.toggle("normal")
        return false;
    });
    
    model.addListener(update);
          
    /** load the tables with the given properties */
    function loadTables(properties) {
        if (properties && properties['type']) {
            nameHeader.html(properties['type']+': '+self.pathname)
            inputs.loadData(properties['Inputs'])
            outputs.loadData(properties['Outputs'])
            
            /** experiment using dat.GUI * /
            var inp_gui = new dat.GUI({ autoPlace: false });
            inputsDiv.html('')
            inputsDiv[0].appendChild(inp_gui.domElement);
            var inp = {};
            jQuery.each(properties['Inputs'],function(idx,obj) {
                var name = obj['name'],
                    val = obj['value'];
                debug.info(obj,name,val);
                if (!val || val['py/object']) { val = '' };
                inp[name] = val;
                inp_gui.add(inp,name);
            })
            debug.info('inp',inp)
            
            var out_gui = new dat.GUI({ autoPlace: false });
            outputsDiv.html('')
            outputsDiv[0].appendChild(out_gui.domElement);
            var out = {};
            jQuery.each(properties['Outputs'],function(idx,obj) {
                var name = obj['name'],
                    val = obj['value'];
                debug.info(obj,name,val);
                if (!val || val['py/object']) { val = '' };
                out[name] = val;
                out_gui.add(out,name);
            })
            debug.info('out',out)    
            /**/
        }
        else {
            nameHeader.html(self.pathname)
            inputs.loadData([])
            outputs.loadData([])
        }
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
        if (self.pathname !== path) {
            self.pathname = path
            inputs.pathname = path;
            outputs.pathname = path;
        }
        model.getComponent(path, loadTables,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = '';
                if (self.elm.parent().hasClass('ui-dialog')) {
                    self.close();
                }
                else {
                    loadTables({});
                }
            }
        )
        return this
    }
    
}

/** set prototype */
openmdao.PropertiesEditor.prototype = new openmdao.BaseFrame();
openmdao.PropertiesEditor.prototype.constructor = openmdao.PropertiesEditor;