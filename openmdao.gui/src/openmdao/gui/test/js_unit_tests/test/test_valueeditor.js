TestCase("ValueEditorTest", {
    setUp : function()
    {
        EditorTestUtils.resetOptions();
    },

    tearDown : function(){},

    "test_option_default_values" : function(){
        assertTrue(openmdao !== undefined );
        assertTrue(openmdao.ValueEditor.defaultEditorEnabled);
        assertFalse(openmdao.ValueEditor.overridesEnabled);
        assertFalse(openmdao.ValueEditor.promptEnabled);
        assertEquals(openmdao.ValueEditor.defaultEditor, Slick.Editors.Text);
    },

    "test_default_editor_enabled" : function(){
        var testObject = EditorTestUtils.getSimpleTestObject();
        var row_0 = 0;
        var row_1 = 1;
        var cellIndex = 0;
        var editorSelector = ".editor-text";

        assertTrue(
                EditorTestUtils.canLoadEditor(
                    testObject, 
                    row_0, 
                    cellIndex, 
                    editorSelector));

        assertTrue(
                EditorTestUtils.canLoadEditor(
                    testObject, 
                    row_1, 
                    cellIndex, 
                    editorSelector));
        
        EditorTestUtils.destroyTestObject(testObject);
    },

    "test_default_editor_disabled" : function(){
        var row_0 = 0;
        var row_1 = 1;
        var cellIndex = 0;
        var editorSelector = ".editor-text";
        var editorOptions = {
            defaultEditorEnabled : false,
        };

        var testObject = EditorTestUtils.getSimpleTestObject();

        EditorTestUtils.setOptions(editorOptions);

        assertFalse(
                EditorTestUtils.canLoadEditor(
                    testObject, 
                    row_0, 
                    cellIndex, 
                    editorSelector));

        assertFalse(
                EditorTestUtils.canLoadEditor(
                    testObject, 
                    row_1, 
                    cellIndex, 
                    editorSelector));
        
        EditorTestUtils.destroyTestObject(testObject);
    },

    "test_registration" : function(){
        var types = ["int", "str"];
        var editors = {
            "int" : Slick.Editors.Text,
            "str" : Slick.Editors.Text,
        };

        assertFalse(EditorTestUtils.areRegistered(types));

        EditorTestUtils.registerEditors(editors);

        assertTrue(EditorTestUtils.areRegistered(types));

        EditorTestUtils.unregisterEditors(types);

        assertFalse(EditorTestUtils.areRegistered(types));

    },

    "test_registered_editors" : function(){
        var editors = {
            "int" : Slick.Editors.Text,
            "str" : Slick.Editors.Text,
        };

        var testObject = EditorTestUtils.getSimpleTestObject(editors);
        var row_0 = 0;
        var row_1 = 1;
        var cellIndex = 0;
        var editorSelector = ".editor-text";
        var editorOptions = {
            defaultEditorEnabled : true,
        }

        EditorTestUtils.setOptions(editorOptions);

        assertTrue(
                EditorTestUtils.canLoadEditor(
                    testObject, 
                    row_0, 
                    cellIndex, 
                    editorSelector));

        assertTrue(
                EditorTestUtils.canLoadEditor(
                    testObject, 
                    row_1, 
                    cellIndex, 
                    editorSelector));
        
        EditorTestUtils.destroyTestObject(testObject);
    },
});
