
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ObjectivesPane = function(elm,model,pathname,name,editable) {
    var objectives,
        objectivesDiv = jQuery("<div id='"+name+"_objectives' >"),
        addButton = jQuery("<div>Add Objective</div>"),
        clrButton = jQuery("<div>Clear Objectives</div>"),        
        columns = [
            {id:"expr",  name:"Expression",  field:"expr", editor:TextCellEditor},
            {id:"scope", name:"Scope", field:"scope", editor:TextCellEditor},
            {id:"name",  name:"Name",  field:"name", editor:TextCellEditor},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        };
        
    elm.append(objectivesDiv).width('100%');
    
    var table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    elm.append(table);
    
    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    objectives = new Slick.Grid(objectivesDiv, [], columns, options)
    if (editable) {
        objectives.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** add a new objective */
    function addObjective(expr,name,scope) {
        cmd = "top."+pathname+".add_objective('"+expr+"'";
        if (name) {
            cmd = cmd + ",name="+name;
        }            
        if (scope) {
            cmd = cmd + ",scope="+scope;
        }            
        cmd = cmd + ");"
        model.issueCommand(cmd);
    };
   
    /** prompt for new objective */
    function promptForObjective(callback) {
        // Build dialog markup
        var win = jQuery('<div></div>'),
            expr   = jQuery('<input type="text" style="width:100%"></input>'),
            name   = jQuery('<input type="text" style="width:50%"></input>'),
            scope  = jQuery('<input type="text" style="width:50%"></input>');
        
        win.append(jQuery('<div>Expression: </div>').append(expr));
        
        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Name: </div>').append(name)))
                            .append(jQuery('<td>').append(jQuery('<div>Scope: </div>').append(scope)));
        table.append(row);
        win.append(table);

        
        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Objective',
            'buttons': {
                'Ok': function() {                    
                    jQuery(this).dialog('close');
                    callback(expr.val(),name.val(),scope.val());
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    };

    /** clear all objectives */
    function clearObjectives() {
        cmd = "top."+pathname+".clear_objectives();"
        model.issueCommand(cmd);        
    }
     
    addButton.click(function() { promptForObjective(addObjective) });
    clrButton.click(function() { clearObjectives() });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            objectives.setData(properties)
        }
        else {
            objectives.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        objectives.updateRowCount()
        objectives.render()
    }    
}