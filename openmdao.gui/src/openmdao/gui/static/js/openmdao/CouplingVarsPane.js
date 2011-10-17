
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.CouplingVarsPane = function(elm,model,pathname,name,editable) {
    var couplingVars,
        couplingVarsDiv = jQuery("<div id='"+name+"_CouplingVars' >"),
        addButton = jQuery("<div>Add Coupling Variables</div>"),
        clrButton = jQuery("<div>Clear Coupling Variables</div>"),        
        columns = [
            {id:"independent", name:"Independent",  field:"independent",  width:180},
            {id:"dependent",   name:"Dependent",    field:"dependent", width:180},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        };
        
    elm.append(couplingVarsDiv).width('100%');
    
    var table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    elm.append(table);
    
    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    couplingVars = new Slick.Grid(couplingVarsDiv, [], columns, options)
    if (editable) {
        couplingVars.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** add a new objective */
    function addCouplingVars(indep,dep,scope) {
        cmd = "top."+pathname+".add_coupling_var('"+indep+"','"+dep+")'";
        model.issueCommand(cmd);
    };
   
    /** prompt for new objective */
    function promptForCouplingVars(callback) {
        // Build dialog markup
        var win = jQuery('<div></div>'),
            indep = jQuery('<input type="text" style="width:100%"></input>'),
            dep   = jQuery('<input type="text" style="width:100%"></input>');
        
        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Independent: </div>').append(indep)))
                            .append(jQuery('<td>').append(jQuery('<div>Dependent:   </div>').append(dep)));
        table.append(row);
        win.append(table);

        
        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Coupling Variables',
            'buttons': {
                'Ok': function() {                    
                    jQuery(this).dialog('close');
                    callback(indep.val(),dep.val());
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    };

    /** clear all CouplingVars */
    function clearCouplingVars() {
        cmd = "top."+pathname+".clear_coupling_vars();"
        model.issueCommand(cmd);        
    }
     
    addButton.click(function() { promptForCouplingVars(addCouplingVars) });
    clrButton.click(function() { clearCouplingVars() });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            couplingVars.setData(properties)
        }
        else {
            couplingVars.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        couplingVars.updateRowCount()
        couplingVars.render()
    }    
}