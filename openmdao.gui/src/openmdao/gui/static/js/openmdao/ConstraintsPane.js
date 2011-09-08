
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ConstraintsPane = function(elm,model,pathname,name,editable) {
    var constraints,
        constraintsDiv = jQuery("<div id='"+name+"_constraints' >"),
        addButton = jQuery("<div>Add Constraint</div>"),
        clrButton = jQuery("<div>Clear Constraints</div>"),   
        columns = [
            {id:"expr",  name:"Expression",  field:"expr", editor:TextCellEditor},
            {id:"scaler", name:"Scaler", field:"scaler", editor:TextCellEditor},
            {id:"adder", name:"Adder", field:"adder", editor:TextCellEditor},
            {id:"name",  name:"Name",  field:"name", editor:TextCellEditor},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        };
        
    elm.append(constraintsDiv);
    
    var table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    elm.append(table);    
    
    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    constraints = new Slick.Grid(constraintsDiv, [], columns, options)
    if (editable) {
        constraints.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** add a new constraint */
    function addConstraint(expr,scaler,adder,name,scope) {
        cmd = "top."+pathname+".add_constraint('"+expr+"'";
        if (scaler) {
            cmd = cmd + ",scaler="+scaler;
        }            
        if (adder) {
            cmd = cmd + ",adder="+adder;
        }  
        if (name) {
            cmd = cmd + ",name="+name;
        }            
        if (scope) {
            cmd = cmd + ",scope="+scope;
        }            
        cmd = cmd + ");"
        model.issueCommand(cmd);
    };
   
    /** prompt for new constraint */
    function promptForConstraint(callback) {
        // Build dialog markup
        var win = jQuery('<div></div>'),
            expr   = jQuery('<input type="text" style="width:100%"></input>'),
            scaler = jQuery('<input type="text" style="width:50%"></input>'),
            adder  = jQuery('<input type="text" style="width:50%"></input>'),
            name   = jQuery('<input type="text" style="width:50%"></input>'),
            scope  = jQuery('<input type="text" style="width:50%"></input>');
        
        win.append(jQuery('<div>Expression: </div>').append(expr));
        
        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Scaler: </div>').append(scaler)))
                            .append(jQuery('<td>').append(jQuery('<div>Adder: </div>').append(adder)));                            
        table.append(row);
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Name: </div>').append(name)))
                            .append(jQuery('<td>').append(jQuery('<div>Scope: </div>').append(scope)));
        table.append(row);
        win.append(table);

        
        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Constraint',
            'buttons': {
                'Ok': function() {                    
                    jQuery(this).dialog('close');
                    callback(expr.val(),scaler.val(),adder.val(),name.val(),scope.val());
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    };

    /** clear all constraints */
    function clearConstraints() {
        cmd = "top."+pathname+".clear_constraints();"
        model.issueCommand(cmd);        
    }
     
    addButton.click(function() { promptForConstraint(addConstraint) });
    clrButton.click(function() { clearConstraints() });
    
    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            constraints.setData(properties)
        }
        else {
            constraints.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        constraints.updateRowCount()
        constraints.render()
    }    
}