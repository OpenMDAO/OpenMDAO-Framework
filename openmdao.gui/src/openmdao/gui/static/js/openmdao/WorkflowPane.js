
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.WorkflowPane = function(elm,model,pathname,name,editable) {
    var objectives,
        objectivesDiv = jQuery("<div id='"+name+"_objectives' >"),
        addButton = jQuery("<div>Add Component</div>"),
        opnButton = jQuery("<div>Open Workflow</div>"),
        clrButton = jQuery("<div>Clear Workflow</div>"),        
        columns = [
            {id:"name",  name:"Name",  field:"name", width:140 },
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        };
        
    elm.append(objectivesDiv).width('100%');
    
    var table = jQuery('<table width="100%">'),
        row = jQuery('<tr>')
                .append(jQuery('<td style="text-align:left">').append(addButton))
                .append(jQuery('<td style="text-align:center">').append(opnButton))
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

    // make the workflow pane droppable
    elm.droppable ({
        accept: '.obj',
        drop: function(ev,ui) { 
            debug.info("WorkflowPane drop:",ev,ui)
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone(),
                path = droppedObject.attr("path"),
                parentPath = openmdao.Util.getParentPath(pathname);
            
            if (path.indexOf(parentPath) == 0) {
                path = path.substr(parentPath.length+1)
            }
            if (path) {
                addComponent(path);
            }
            else {
                debug.warn('WorkflowPane received unknown drop:',droppedObject,path);
            }
        }
    });
    
    /** add a new objective */
    function addComponent(name) {
        cmd = "top."+pathname+".workflow.add('"+name+"');"
        model.issueCommand(cmd);
    };
   
    /** prompt for new objective */
    function promptForComponent(callback) {
        // Build dialog markup
        var win = jQuery('<div></div>'),
            name = jQuery('<input type="text" style="width:50%"></input>');
        
        win.append(jQuery('<div>Name: </div>').append(name));
        
        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'Add to Workflow',
            'buttons': {
                'Ok': function() {                    
                    jQuery(this).dialog('close');
                    callback(name.val());
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    };

    /** clear workflow */
    function clearWorkflow() {
        cmd = "top."+pathname+".workflow.clear();"
        model.issueCommand(cmd);        
    }
    
    /** add a new objective */
    function openWorkflow() {
        var id = pathname.replace(/\./g,'-');
        new openmdao.WorkflowDiagram(id,model,pathname).showWorkflow(pathname);
    };
   
     
    addButton.click(function() { promptForComponent(addComponent) });
    opnButton.click(function() { openWorkflow() });
    clrButton.click(function() { clearWorkflow() });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            var tableData = [];
            jQuery.each(properties,function (idx,name) {
                tableData.push({'name':name});
            });
            objectives.setData(tableData);
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