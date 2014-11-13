/* Test the functions in Project.js */
/* Uses the Sinon library http://sinonjs.org/ */


TestCase("ProjectTest", {
    setUp: function() {
        openmdao.project = new openmdao.Project();

        this.fakeXhr = sinon.useFakeXMLHttpRequest();
        var requests = this.requests = [];

        this.fakeXhr.onCreate = function(xhr) {
            requests.push(xhr);
       };
    },

    tearDown: function() {
        this.fakeXhr.restore();
    },

    checkStandardRequestInfo: function(requests) {
        assertEquals("GET", requests[0].method);
        assertEquals(true, requests[0].async);
        assertEquals(1, requests.length);
    },

    checkStandardCallbackBehavior: function(success, error) {
        sinon.assert.calledOnce(success);
        sinon.assert.notCalled(error);
        assertEquals(success.exceptions[0], undefined);
    },

    "test addListener": function() {
        callback1 = sinon.spy();
        callback2 = sinon.spy();

        // addListener will make an ajax call telling the server to publish
        // that topic
        openmdao.project.addListener('some.path.name', callback1);
        assertEquals("subscription/some.path.name", this.requests[0].url);
        assertEquals("GET", this.requests[0].method);
        assertEquals(true, this.requests[0].async);
        assertEquals(null, this.requests[0].requestBody);

        assertEquals(1, this.requests.length);
    },

    "test getTypes": function() {
        var type_info = "";

        var success_handler = sinon.spy(function(info) {
            type_info = info;
        });

        var error_handler = sinon.spy(function(jqXHR, textStatus, errorThrown) {
        });

        openmdao.project.getTypes()
            .done(success_handler)
            .fail(error_handler);

        // Check the requests
        assertEquals("types", this.requests[0].url);
        this.checkStandardRequestInfo(this.requests);

        // Set the response
        this.requests[0].respond(200,
            {"Content-Type": "application/json"},
            '{"working": {}}'
        );

        // Check the callbacks
        this.checkStandardCallbackBehavior(success_handler, error_handler);
        assert(success_handler.calledWith({"working": {}}));

        // The bottom line. Did we get the expected output?
        assertEquals(type_info, {"working": {}});
    },

    "test commitProject": function() {
        callback1 = sinon.spy();
        openmdao.project.addListener('', callback1);

        openmdao.project.commit_with_comment('a comment');

        // Check the requests
        assertEquals("project", this.requests[0].url);
        assertEquals("POST", this.requests[0].method);
        assertEquals(true, this.requests[0].async);
        assertEquals(1, this.requests.length);
        assertEquals("action=commit&comment=a+comment", this.requests[0].requestBody);

        sinon.assert.notCalled(callback1);

        // Set the response
        this.requests[0].respond(200, {}, '');

        // committing project has no side effects, so no callbacks at this time
        sinon.assert.notCalled(callback1);
    },

    "test getWorkflow": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        openmdao.project.getWorkflow("driverpath")
            .done(success_handler)
            .fail(error_handler);

        // Check the requests
        assertEquals("object/driverpath/workflow", this.requests[0].url);
        this.checkStandardRequestInfo(this.requests);

        // Set the response
        this.requests[0].respond(200,
            {"Content-Type": "application/json"},
            '{"id" : 1223, "name" : "workflowname"}'
        );

        // Check the callbacks
        this.checkStandardCallbackBehavior(success_handler, error_handler);
        assert(success_handler.calledWith({"id" : 1223, "name": "workflowname"}));

        // test call of error handler
        openmdao.project.getWorkflow("workflowpath")
            .done(success_handler)
            .fail(error_handler);

        this.requests[1].respond(500, {"Content-Type": "application/json"}, '{}');
        sinon.assert.calledOnce(success_handler);
        sinon.assert.calledOnce(error_handler);

        // test what happens when workflowpathname is None
        openmdao.project.getWorkflow("None")
            .done(success_handler)
            .fail(error_handler);

        assertEquals("object/None/workflow", this.requests[2].url);
        assertEquals(this.requests.length, 3);
        this.requests[2].respond(200,
            {"Content-Type": "application/json"},
            '{"id" : 1223, "name" : "workflowname"}'
        );
        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test getComponents": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        openmdao.project.getComponents()
            .done(success_handler)
            .fail(error_handler);

        // Check the requests
        assertEquals("objects", this.requests[0].url);
        this.checkStandardRequestInfo(this.requests);

        // Set the response
        this.requests[0].respond(200,
            {"Content-Type": "application/json"},
            '{"id": 1223, "name": "componentname"}'
        );
        // Check the callbacks
        this.checkStandardCallbackBehavior(success_handler, error_handler);
        assert(success_handler.calledWith({"id": 1223, "name": "componentname"}));

        // test call of error handler
        openmdao.project.getComponents()
            .done(success_handler)
            .fail(error_handler);

        this.requests[1].respond(500,
            {"Content-Type": "application/json"},
            '{}'
        );
        sinon.assert.calledOnce(success_handler);
        sinon.assert.calledOnce(error_handler);

        // test what happens when componentspathname is empty
        openmdao.project.getComponents()
            .done(success_handler)
            .fail(error_handler);
        assertEquals("objects", this.requests[2].url);
        assertEquals(this.requests.length, 3);
        this.requests[2].respond(200,
            {"Content-Type": "application/json"},
            '{"id" : 1223, "name" : "componentsname"}'
        );
        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test getObject": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        openmdao.project.getObject("componentname")
            .done(success_handler)
            .fail(error_handler);

        // Check the requests
        assertEquals("object/componentname", this.requests[0].url);
        this.checkStandardRequestInfo(this.requests);

        // Set the response
        this.requests[0].respond(200,
            {"Content-Type": "application/json"},
            '{"id" : 1223, "name" : "componentname"}'
        );
        // Check the callbacks
        this.checkStandardCallbackBehavior(success_handler, error_handler);
        assert(success_handler.calledWith({"id": 1223, "name": "componentname"}));

        // test call of error handler
        openmdao.project.getObject("componentname")
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(500,
            {"Content-Type": "application/json"},
            '{}'
        );
        sinon.assert.calledOnce(success_handler);
        sinon.assert.calledOnce(error_handler);

        // test what happens when pathname is empty
        openmdao.project.getObject("")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("object/", this.requests[2].url);
        assertEquals(this.requests.length, 3);
        this.requests[2].respond(200,
            {"Content-Type": "application/json"},
            '{"id" : 1223, "name" : "componentsname"}'
        );
        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test putObject": function() {
        var callback = sinon.spy();

        // Normal execution
        openmdao.project.putObject("component_name", "typepath", "")
            .done(callback);
        assertEquals("object/component_name", this.requests[0].url);
        assertEquals("PUT", this.requests[0].method);
        assertEquals("type=typepath&args=", this.requests[0].requestBody);
        this.requests[0].respond(200, 'response', '');
        sinon.assert.calledOnce(callback);
        assertEquals(callback.args[0][0], "");

        // Are listeners updated?
        openmdao.project.putObject("typepath", "component_name", "")
            .done(callback);
        this.requests[1].respond(200, 'response', '');  // the ajax call just queues up the request
        sinon.assert.calledTwice(callback);
    },

    "test issueCommand": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        // Normal execution
        openmdao.project.issueCommand("command")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("command", this.requests[0].url);
        assertEquals("POST", this.requests[0].method);
        assertEquals("command=command", this.requests[0].requestBody);
        this.requests[0].respond(200,
            {"Content-Type": "application/json"},
            '{"status" : "OK"}'
        );
        sinon.assert.calledOnce( success_handler);
        assertEquals({"status" : "OK"}, success_handler.args[0][0]);

        // Are listeners updated?
        openmdao.project.issueCommand("command")
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(200, 'response', '');  // the ajax call just queues up the request
        sinon.assert.calledTwice(success_handler);

        // Does error handler get called?
        openmdao.project.issueCommand("command")
            .done(success_handler)
            .fail(error_handler);
        this.requests[2].respond(500,
            {"Content-Type": "application/json"},
            '{}'
       );

        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test setVariableValue": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        // Normal execution
        openmdao.project.setVariableValue("varname", "value", "vtype")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("variable/varname", this.requests[0].url);
        assertEquals("POST", this.requests[0].method);
        assertEquals("rhs=value&type=vtype", this.requests[0].requestBody);
        this.requests[0].respond(200, {"Content-Type": "application/json"}, '{"status" : "OK"}');
        sinon.assert.calledOnce(success_handler);
        assertEquals({"status" : "OK"}, success_handler.args[0][0]);

        // Are listeners updated?
        openmdao.project.setVariableValue("varname","value","vtype")
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(200, 'response', ''); // the ajax call just queues up the request
        sinon.assert.calledTwice(success_handler);

        // Does error handler get called?
        openmdao.project.setVariableValue("varname", "value", "vtype")
            .done(success_handler)
            .fail(error_handler);
        this.requests[2].respond(500, {"Content-Type": "application/json"}, '{}');

        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test getFiles": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        // Normal operation
        openmdao.project.getFiles()
            .done(success_handler)
            .fail(error_handler);
        assertEquals("files", this.requests[0].url);
        assertEquals(null, this.requests[0].requestBody);
        this.checkStandardRequestInfo(this.requests);
        this.requests[0].respond(200, {"Content-Type": "application/json"},
                               '{"files": [ "file1", "file2" ]} ');
        this.checkStandardCallbackBehavior(success_handler, error_handler);
        assert(success_handler.calledWith({"files": ["file1", "file2"]}));

        // Check error handler
        openmdao.project.getFiles()
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(500, {"Content-Type": "application/json"}, '{}');
        sinon.assert.calledOnce(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test getFile": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        // Normal operation
        openmdao.project.getFile("filepath")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("filefilepath", this.requests[0].url);
        assertEquals(null, this.requests[0].requestBody);
        this.checkStandardRequestInfo(this.requests);
        this.requests[0].respond(200, {"Content-Type": "text/plain"},
                                 'file contents');
        this.checkStandardCallbackBehavior(success_handler, error_handler);
        assert(success_handler.calledWith("file contents"));

        // Check error handler
        openmdao.project.getFile("filepath")
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(500, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledOnce(success_handler);
        sinon.assert.calledOnce(error_handler);

        // test what happens when backslash is in filepath
        openmdao.project.getFile("file\\path")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("filefile/path", this.requests[2].url);
        assertEquals(this.requests.length, 3);
        this.requests[2].respond(200, {"Content-Type": "text/plain"},
                               'file contents');
        sinon.assert.calledTwice(success_handler);
    },

    "test setFile": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        // Normal operation
        openmdao.project.setFile("filepath", "file contents", undefined)
            .done(success_handler)
            .fail(error_handler);
        assertEquals("file/filepath", this.requests[0].url);
        assertEquals(this.requests[0].method, "PUT");
        assertEquals("contents=file+contents", this.requests[0].requestBody);
        this.requests[0].respond(200, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledOnce(success_handler);
        sinon.assert.notCalled(error_handler);

        // Check error handler
        openmdao.project.setFile("filepath", "file contents", undefined)
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(500, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledOnce(success_handler);
        sinon.assert.calledOnce(error_handler);

        // test what happens when backslash is in filepath
        openmdao.project.setFile("file\\path", "file contents", undefined)
            .done(success_handler)
            .fail(error_handler);
        assertEquals("file/file/path", this.requests[2].url);
        assertEquals(this.requests.length, 3);
        this.requests[2].respond(200, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test createFolder": function() {
        var success_handler = sinon.spy();
        var error_handler = sinon.spy();

        // Normal operation
        openmdao.project.createFolder("folderpath")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("file/folderpath", this.requests[0].url);
        assertEquals(this.requests[0].method, "PUT");
        assertEquals("isFolder=true", this.requests[0].requestBody);
        this.requests[0].respond(200, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledOnce(success_handler);
        sinon.assert.notCalled(error_handler);

        // Check error handler
        openmdao.project.createFolder("folderpath")
            .done(success_handler)
            .fail(error_handler);
        this.requests[1].respond(500, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledOnce(error_handler);
        sinon.assert.calledOnce(success_handler);

        // test what happens when backslash is in folderpath
        openmdao.project.createFolder("folder\\path")
            .done(success_handler)
            .fail(error_handler);
        assertEquals("file/folder/path", this.requests[2].url);
        assertEquals(this.requests.length, 3);
        this.requests[2].respond(200, {"Content-Type": "text/plain"}, '');
        sinon.assert.calledTwice(success_handler);
        sinon.assert.calledOnce(error_handler);
    },

    "test newFile": function() {
        // Normal operation with JSON new file name
        openmdao.project.newFile("newfilename.json", "folderpath");
        // It calls setFile so check to see if that happens as expected
        assertEquals("file/folderpath/newfilename.json", this.requests[0].url);
        assertEquals(this.requests[0].method, "PUT");
        assertEquals("contents=%5B%5D", this.requests[0].requestBody);
        this.requests[0].respond(200, {"Content-Type": "text/plain"}, '');

        // Normal operation with .py new file name
        openmdao.project.newFile("newfilename.py", "folderpath");
        // It calls setFile so check to see if that happens as expected
        assertEquals("file/folderpath/newfilename.py", this.requests[1].url);
        assertEquals(this.requests[1].method, "PUT");
        assertEquals("contents=%22%22%22%0A+++folderpath%2Fnewfilename.py%0A%22%22%22%0A%0A",
           this.requests[1].requestBody);
        this.requests[1].respond(200, {"Content-Type": "text/plain"}, '');

        // Normal operation with no extension on file name
        openmdao.project.newFile("newfilename", "folderpath");
        // It calls setFile so check to see if that happens as expected
        assertEquals("file/folderpath/newfilename", this.requests[2].url);
        assertEquals(this.requests[2].method, "PUT");
        assertEquals("contents=", this.requests[2].requestBody);
        this.requests[2].respond(200, {"Content-Type": "text/plain"}, '');
    },

    "test newFolder": function() {
        // Normal operation
        openmdao.project.newFolder("newfoldername", "folderpath");
        // It calls createFolder so check to see if that happens as expected
        assertEquals("file/folderpath/newfoldername", this.requests[0].url);
        assertEquals(this.requests[0].method, "PUT");
        assertEquals("isFolder=true", this.requests[0].requestBody);
        this.requests[0].respond(200, {"Content-Type": "text/plain"}, '');
    },

    "test removeFile": function() {
        // Normal operation
        openmdao.project.removeFile("filepath");
        assertEquals("filefilepath", this.requests[0].url);
        assertEquals(this.requests[0].method, "DELETE");
        assertEquals("file=filepath", this.requests[0].requestBody);
        this.requests[0].respond(200, {"Content-Type": "text/plain"}, '');

        // TODO: I do not see how to check the error handling at this point
        // Unless I do some mocking of the debug.warn
    },

    "test execFile": function() {
      // Normal execution
      openmdao.project.execFile("/path\\filename");
      assertEquals("file/path/filename", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      this.requests[0].respond(200, {"Content-Type": "text/plain"}, 'OK');
    }

});
