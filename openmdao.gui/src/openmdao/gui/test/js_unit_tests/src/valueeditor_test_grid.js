EditorTestUtils = {}

EditorTestUtils.appendTestContainer = function(container){
    document.body.appendChild(container);
}

EditorTestUtils.removeTestContainer = function(container){
    document.body.removeChild(container);
}

EditorTestUtils.getSimpleTestObject = function(registration){

    var container = document.createElement("div");
    container.id = "test_div";

    var data = [
        {name:"one", type:"int"},
        {name:"two", type:"str"},
    ];

    var columns = [
        {id:"name", name:"Name", field:"name", width:80, editor:openmdao.ValueEditor},
        {id:"type", name:"Type", field:"type", width:80}
    ];

    var options = {
        editable : true,
        asyncEditorLoading : false,
        multiSelect : false,
        autoHeight : true,
        enableTextSelectionOnCells : true
    };

    registration = (registration) ? registration : {};
    
    return new EditorTestUtils.TestObject(container, data, columns, options, registration);
}

EditorTestUtils.resetOptions = function(){
    openmdao.ValueEditor.defaultEditorEnabled = true;
    openmdao.ValueEditor.overridesEnabled = false;
    openmdao.ValueEditor.promptEnabled = false;
    openmdao.ValueEditor.defaultEditor = Slick.Editors.Text;
    
}

EditorTestUtils.setOptions = function(editorOptions){
    for(option in editorOptions){
        openmdao.ValueEditor[option] = editorOptions[option];
    }
}

EditorTestUtils.TestObject = function(container, data, columns, options, registration){

    EditorTestUtils.appendTestContainer(container);
    EditorTestUtils.registerEditors(registration);

    this.container = container;
    this.grid = new Slick.Grid(container, data, columns, options);
    this.types = Object.keys(registration);

}

EditorTestUtils.TestObject.prototype = {

    getRow : function(rowIndex){
        return jQuery('.slick-row')[rowIndex];
    },

    getCell : function(row, cellIndex){
        return row.childNodes[cellIndex];
    },

    launchCellEditor : function(cell){
        jQuery(cell).dblclick();
    },

    cancelEdit : function(editor){
        var e = jQuery.Event("keydown", {which : 27});
        jQuery(editor).trigger(e);
    },

    getCellEditor : function(selector){
        return jQuery(selector)[0];
    },
}

EditorTestUtils.destroyTestObject = function(testObject){
    EditorTestUtils.removeTestContainer(testObject.container);
    EditorTestUtils.unregisterEditors(testObject.types);
    delete testObject;
}

EditorTestUtils.registerEditors = function(hash){
    for(type in hash){
        openmdao.ValueEditor.registerEditor(type, hash[type]);
    }
}

EditorTestUtils.unregisterEditors = function(array){
    for(i=0; i<array.length; i++){
        openmdao.ValueEditor.unregisterEditor(array[i]);
    }
}

EditorTestUtils.areRegistered = function(array){
    for(i=0; i<array.length; i++){
        if(!openmdao.ValueEditor.isRegistered(array[i])){
            return false;
        }
    }

    return true;
}

EditorTestUtils.canLoadEditor = function(testObject, rowIndex, cellIndex, editorSelector){
    var getRow = testObject.getRow;
    var getCell = testObject.getCell;
    var launchCellEditor = testObject.launchCellEditor;
    var getCellEditor = testObject.getCellEditor;
    
    launchCellEditor(getCell(getRow(rowIndex), cellIndex));

    var cellEditor = getCellEditor(editorSelector);

    if(cellEditor){
        testObject.cancelEdit(cellEditor);
    }

    return (cellEditor) ? true : false;
}
