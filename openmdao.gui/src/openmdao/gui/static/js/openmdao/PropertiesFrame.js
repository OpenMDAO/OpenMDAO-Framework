
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PropertiesFrame = function(id,model) {
    openmdao.PropertiesFrame.prototype.init.call(this,id,'Properties');

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
        outputsDiv = jQuery("<div id='outputs'>");

    this.elm.append(nameHeader);
    this.elm.append('<p>');
    this.elm.append(inputsHeader);
    this.elm.append(inputsDiv);
    this.elm.append('<p>');
    this.elm.append(outputsHeader);
    this.elm.append(outputsDiv);
    this.elm.width(200);

    inputs = new openmdao.PropertiesPane(inputsDiv,model,self.pathname,'Inputs',true);
    inputsHeader.click(function () {
        inputsDiv.toggle("normal");
        return false;
    });

    outputs = new openmdao.PropertiesPane(outputsDiv,model,self.pathname,'Outputs',false);
    outputsHeader.click(function () {
        outputsDiv.toggle("normal");
        return false;
    });

    /** load the tables with the given properties */
    function loadTables(properties) {
        if (properties && properties.type) {
            nameHeader.html(properties.type+': '+self.pathname);
            inputs.loadData(properties.Inputs);
            outputs.loadData(properties.Outputs);
        }
        else {
            nameHeader.html(self.pathname);
            inputs.loadData([]);
            outputs.loadData([]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get the specified object from model, load properties into table */
    this.editObject = function(path) {
        if (self.pathname !== path) {
            self.pathname = path;
            inputs.pathname = path;
            outputs.pathname = path;
            model.addListener(self.pathname,loadTables);
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
        );
        return this;
    };

    /** if there is an object loaded, update it from the model */
    this.update = function() {
        if (self.pathname && self.pathname.length>0) {
            self.editObject(self.pathname);
        }
    };

    //model.addListener('',this.update);
};

/** set prototype */
openmdao.PropertiesFrame.prototype = new openmdao.BaseFrame();
openmdao.PropertiesFrame.prototype.constructor = openmdao.PropertiesFrame;

