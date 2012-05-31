/* Test the functions in Model.js */
/* Uses the Sinon library http://sinonjs.org/ */


TestCase("ModelTest", {
  setUp: function () {
    openmdao.model = new openmdao.Model();

    // stub Util.openWebSocket
    if ( ! this.websocketStub ) {
        this.websocketStub = sinon.stub(openmdao.Util, "openWebSocket",
              function( addr, handler ) {
                    // We don't need no steenkin' web sockets
                  return;
              }
        );
     }

    this.fakeXhr = sinon.useFakeXMLHttpRequest();
    var requests = this.requests = [];

    this.fakeXhr.onCreate = function (xhr) {
      requests.push(xhr);
    };
  },

  tearDown: function () {
    this.websocketStub.restore();
    this.fakeXhr.restore();
  },

  checkStandardRequestInfo: function (requests) {
      assertEquals("GET", requests[0].method);
      assertEquals(true, requests[0].async);
      assertEquals(1, requests.length);
  },

  checkStandardCallbackBehavior: function (success, error) {
      sinon.assert.calledOnce(success) ;
      sinon.assert.notCalled( error );
      assertEquals( success.exceptions[0], undefined ) ;
  },


  "test addListener": function () {

      callback1 = sinon.spy() ;
      callback2 = sinon.spy() ;

      // 1st listener to outstream will initialize the outstream websocket
      openmdao.model.addListener('outstream', callback1);
      assertEquals("outstream", this.requests[0].url);
      assertEquals("GET", this.requests[0].method);
      assertEquals(true, this.requests[0].async);
      assertEquals(1, this.requests.length);


      // 1st listener to any other topic will initialize the pubstream
      // and then make an ajax call telling the server to publish that topic
      openmdao.model.addListener('somepathname', callback1);
      assertEquals("pubstream", this.requests[1].url);
      assertEquals("GET", this.requests[1].method);
      assertEquals(true, this.requests[1].async);
      assertEquals(null, this.requests[1].requestBody);


      assertEquals("publish?topic=somepathname&publish=true", this.requests[2].url);
      assertEquals("GET", this.requests[1].method);
      assertEquals(true, this.requests[1].async);

      assertEquals(3, this.requests.length);
  },

  "test getTypes": function () {

      var type_info = "" ;

      var success_handler = sinon.spy(
          function(info) {
              type_info = info ;
          });

      var error_handler = sinon.spy(
          function(jqXHR, textStatus, errorThrown) {
          }
      );

      openmdao.model.getTypes( success_handler, error_handler );

      // Check the requests
      assertEquals("types", this.requests[0].url);
      this.checkStandardRequestInfo(this.requests) ;

      // Set the response
      this.requests[0].respond(200, { "Content-Type": "application/json" },
                               '{"working": {} }' ) ;

      // Check the callbacks
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({"working": {} } ) ) ;

      // The bottom line. Did we get the expected output?
      assertEquals( type_info, {"working": {} } ) ;

  },



  "test newModel": function () {

      callback1 = sinon.spy() ;

      // call newModel
      openmdao.model.newModel();

      // Check the request
      assertEquals("model", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(true, this.requests[0].async);
      assertEquals(1, this.requests.length);
      assertEquals(null, this.requests[0].requestBody);

      // Set the response
      this.requests[0].respond(200, {}, '');

  },


  "test saveProject": function () {

      callback1 = sinon.spy() ;
      openmdao.model.addListener( '', callback1 ) ;

      openmdao.model.saveProject( );

      // Check the requests
      assertEquals("project", this.requests[1].url);
      assertEquals("POST", this.requests[1].method);
      assertEquals(true, this.requests[1].async);
      assertEquals(2, this.requests.length);
      assertEquals(null, this.requests[1].requestBody);

      sinon.assert.notCalled( callback1 );

      // Set the response
      this.requests[1].respond(200, {}, '');

      // saving project has no side effects, so no callbacks at this time
      sinon.assert.notCalled( callback1 );

  },


  "test getWorkflow": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      openmdao.model.getWorkflow( "workflowpath", success_handler, error_handler );

      // Check the requests
      assertEquals("workflow/workflowpath", this.requests[0].url);
      this.checkStandardRequestInfo(this.requests) ;

      // Set the response
      this.requests[0].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "workflowname" }' ) ;
      // Check the callbacks
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({ "id" : 1223, "name" : "workflowname" } ) ) ;

      // test call of error handler
      openmdao.model.getWorkflow( "workflowpath", success_handler, error_handler );
      this.requests[1].respond(500, { "Content-Type": "application/json" }, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when success_handler is not function
      openmdao.model.getWorkflow( "workflowpath", "string not function", error_handler );
      assertEquals( this.requests.length, 2 ) ;
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when workflowpathname is empty
      openmdao.model.getWorkflow( "", success_handler, error_handler );
      assertEquals("workflow/", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "workflowname" }' ) ;
      sinon.assert.calledTwice( success_handler );
      sinon.assert.calledOnce( error_handler );

  },


  "test getComponents": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      openmdao.model.getComponents( success_handler, error_handler );

      // Check the requests
      assertEquals("components", this.requests[0].url);
      this.checkStandardRequestInfo(this.requests) ;

      // Set the response
      this.requests[0].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "componentname" }' ) ;
      // Check the callbacks
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({ "id" : 1223, "name" : "componentname" } ) ) ;

      // test call of error handler
      openmdao.model.getComponents( success_handler, error_handler );
      this.requests[1].respond(500, { "Content-Type": "application/json" }, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when success_handler is not function
      openmdao.model.getComponents( "string not function", error_handler );
      assertEquals( this.requests.length, 2 ) ;
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when componentspathname is empty
      openmdao.model.getComponents( success_handler, error_handler );
      assertEquals("components", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "componentsname" }' ) ;
      sinon.assert.calledTwice( success_handler );
      sinon.assert.calledOnce( error_handler );

  },


  "test getComponent": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      openmdao.model.getComponent( "componentname", success_handler, error_handler );

      // Check the requests
      assertEquals("component/componentname", this.requests[0].url);
      this.checkStandardRequestInfo(this.requests) ;

      // Set the response
      this.requests[0].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "componentname" }' ) ;
      // Check the callbacks
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({ "id" : 1223, "name" : "componentname" } ) ) ;

      // test call of error handler
      openmdao.model.getComponent( "componentname", success_handler, error_handler );
      this.requests[1].respond(500, { "Content-Type": "application/json" }, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when success_handler is not function
      openmdao.model.getComponent( "componentname", "string not function", error_handler );
      assertEquals( this.requests.length, 2 ) ;
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when componentspathname is empty
      openmdao.model.getComponent( "", success_handler, error_handler );
      assertEquals("component/", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "componentsname" }' ) ;
      sinon.assert.calledTwice( success_handler );
      sinon.assert.calledOnce( error_handler );

  },


  "test getConnections": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      openmdao.model.getConnections( "connectionspathname",
                                     "src_name", "dst_name",
                                     success_handler, error_handler );

      // Check the requests
      assertEquals("connections/connectionspathname?src_name=src_name&dst_name=dst_name",
                   this.requests[0].url);
      this.checkStandardRequestInfo(this.requests) ;

      // Set the response
      this.requests[0].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "connectionspathname" }' ) ;
      // Check the callbacks
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({ "id" : 1223, "name" : "connectionspathname" } ) ) ;

      // test call of error handler
      openmdao.model.getConnections( "connectionspathname",
                                     "src_name", "dst_name",
                                     success_handler, error_handler );
      this.requests[1].respond(500, { "Content-Type": "application/json" }, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when success_handler is not function
      openmdao.model.getConnections( "connectionspathname",
                                     "src_name", "dst_name",
                                     "string not function", error_handler );
      assertEquals( this.requests.length, 2 ) ;
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when connections pathname is empty
      openmdao.model.getConnections( "",
                                     "src_name", "dst_name",
                                     success_handler, error_handler );
      this.requests[2].respond(200, { "Content-Type": "application/json" },
                               '{ "id" : 1223, "name" : "connectionspathname" }' ) ;
      assertEquals("connections/?src_name=src_name&dst_name=dst_name",
                   this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      sinon.assert.calledTwice( success_handler );
      sinon.assert.calledOnce( error_handler );

  },


  "test setConnections": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal execution
      openmdao.model.setConnections( "connectionspathname",
                                     "src_name", "dst_name",
                                     "connections",
                                     success_handler, error_handler );
      assertEquals("connections/connectionspathname", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      this.requests[0].respond(200, { }, '' ) ;
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({ } ) ) ;

      // test call of error handler
      openmdao.model.setConnections( "connectionspathname",
                                     "src_name", "dst_name", "connections",
                                     success_handler, error_handler );
      this.requests[1].respond(500, {}, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

  },


  "test addComponent": function () {

      var callback = sinon.spy() ;

      // Normal execution
      openmdao.model.addComponent("typepath","component_name","parent",callback) ;
      assertEquals("component/component_name", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals("type=typepath&parent=parent", this.requests[0].requestBody);
      this.requests[0].respond(200, 'response', '' ) ;
      sinon.assert.calledOnce( callback );
      assertEquals(callback.args[0][0], "" ) ;

      // With null parent
      openmdao.model.addComponent("typepath","component_name",null,callback) ;
      assertEquals("type=typepath&parent=", this.requests[1].requestBody);

      // Are listeners updated?
      openmdao.model.addComponent("typepath","component_name",null,callback) ;
      this.requests[2].respond(200, 'response', '' ) ; // the ajax call just queues up the request
      sinon.assert.calledTwice( callback );

      // Does it handle the "if openmdao.Util" statement properly?
      openmdao.Util.$component_name = function() { } ;
      openmdao.model.addComponent("driver_typepath","component_name",null,callback) ;
      sinon.assert.calledTwice( callback ); // Should not be called this time so still 2 calls

  },


  "test issueCommand": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal execution
      openmdao.model.issueCommand("command",success_handler,error_handler) ;
      assertEquals("command", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals("command=command", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "application/json" }, '{ "status" : "OK"}' ) ;
      sinon.assert.calledOnce( success_handler );
      assertEquals({ "status" : "OK"}, success_handler.args[0][0]) ;

      // Are listeners updated?
      openmdao.model.issueCommand("command",success_handler,error_handler) ;
      this.requests[1].respond(200, 'response', '' ) ; // the ajax call just queues up the request
      sinon.assert.calledTwice( success_handler );

      // Does error handler get called?
      openmdao.model.issueCommand("command",success_handler,error_handler) ;
      this.requests[2].respond(500, { "Content-Type": "application/json" }, '{ }');

      sinon.assert.calledTwice( success_handler );
      sinon.assert.calledOnce( error_handler );

  },

  "test getOutput": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal execution
      openmdao.model.getOutput(success_handler,error_handler) ;
      assertEquals("output", this.requests[0].url);
      assertEquals("GET", this.requests[0].method);
      assertEquals(null, this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "application/json" }, '{ "output" : "output"}' ) ;
      sinon.assert.calledOnce( success_handler );
      assertEquals({ "output" : "output"}, success_handler.args[0][0]) ;

      // Does error handler get called?
      openmdao.model.getOutput(success_handler,error_handler) ;
      this.requests[1].respond(500, { "Content-Type": "application/json" }, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

  },


  "test getFiles": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal operation
      openmdao.model.getFiles( success_handler, error_handler );
      assertEquals("files", this.requests[0].url);
      assertEquals(null, this.requests[0].requestBody);
      this.checkStandardRequestInfo(this.requests) ;
      this.requests[0].respond(200, { "Content-Type": "application/json" },
                               '{ "files": [ "file1", "file2" ] } ' ) ;
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith({ "files": [ "file1", "file2" ] } ) ) ;

      // Check error handler
      openmdao.model.getFiles( success_handler, error_handler );
      this.requests[1].respond(500, { "Content-Type": "application/json" }, '{ }');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when success_handler is not function
      openmdao.model.getFiles( "success_handler", error_handler );
      assertEquals( this.requests.length, 2 ) ;
      sinon.assert.calledOnce( success_handler );

  },


  "test getFile": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal operation
      openmdao.model.getFile( "filepath", success_handler, error_handler );
      assertEquals("filefilepath", this.requests[0].url);
      assertEquals(null, this.requests[0].requestBody);
      this.checkStandardRequestInfo(this.requests) ;
      this.requests[0].respond(200, { "Content-Type": "text/plain" },
                               'file contents' ) ;
      this.checkStandardCallbackBehavior(success_handler, error_handler) ;
      assert(success_handler.calledWith( "file contents" ) );

      // Check error handler
      openmdao.model.getFile( "filepath", success_handler, error_handler );
      this.requests[1].respond(500, { "Content-Type": "text/plain" }, '');
      sinon.assert.calledOnce( success_handler );
      sinon.assert.calledOnce( error_handler );

      // test what happens when success_handler is not function
      openmdao.model.getFile( "filepath", "success_handler", error_handler );
      assertEquals( this.requests.length, 2 ) ;
      sinon.assert.calledOnce( success_handler );

      // test what happens when backslash is in filepath
      openmdao.model.getFile( "file\\path", success_handler, error_handler );
      assertEquals("filefile/path", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "text/plain" },
                               'file contents' ) ;
      sinon.assert.calledTwice( success_handler );

  },

  "test setFile": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal operation
      openmdao.model.setFile("filepath", "file contents",
                             success_handler, error_handler);
      assertEquals("file/filepath", this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("contents=file+contents", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledOnce(success_handler);
      sinon.assert.notCalled(error_handler);

      // Check error handler
      openmdao.model.setFile("filepath", "file contents",
                             success_handler, error_handler);
      this.requests[1].respond(500, { "Content-Type": "text/plain" }, '');
      sinon.assert.calledOnce(success_handler);
      sinon.assert.calledOnce(error_handler);

      // test what happens when backslash is in filepath
      openmdao.model.setFile( "file\\path", "file contents",
                             success_handler, error_handler);
      assertEquals("file/file/path", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledTwice(success_handler);
      sinon.assert.calledOnce(error_handler);

  },

  "test createFolder": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal operation
      openmdao.model.createFolder("folderpath", success_handler, error_handler);
      assertEquals("file/folderpath", this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("isFolder=true", this.requests[0].requestBody);
      this.requests[0].respond(200, {"Content-Type": "text/plain" }, '');
      sinon.assert.calledOnce(success_handler);
      sinon.assert.notCalled(error_handler);

      // Check error handler
      openmdao.model.createFolder("folderpath", success_handler, error_handler);
      this.requests[1].respond(500, { "Content-Type": "text/plain" }, '');
      sinon.assert.calledOnce( error_handler );
      sinon.assert.calledOnce(success_handler);

      // test what happens when backslash is in folderpath
      openmdao.model.createFolder( "folder\\path", success_handler, error_handler);
      assertEquals("file/folder/path", this.requests[2].url);
      assertEquals( this.requests.length, 3) ;
      this.requests[2].respond(200, {"Content-Type": "text/plain" }, '');
      sinon.assert.calledTwice(success_handler);
      sinon.assert.calledOnce(error_handler);

  },

  "test newFile": function () {

      // Normal operation with JSON new file name
      openmdao.model.newFile("newfilename.json","folderpath");
      // It calls setFile so check to see if that happens as expected
      assertEquals("file/folderpath/newfilename.json", this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("contents=%5B%5D", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;

      // Normal operation with .py new file name
      openmdao.model.newFile("newfilename.py", "folderpath");
      // It calls setFile so check to see if that happens as expected
      assertEquals("file/folderpath/newfilename.py", this.requests[1].url);
      assertEquals(this.requests[1].method, "POST" );
      assertEquals("contents=%22%22%22%0A+++folderpath%2Fnewfilename.py%0A%22%22%22%0A%0A",
           this.requests[1].requestBody);
      this.requests[1].respond(200, { "Content-Type": "text/plain" }, '' ) ;

      // Normal operation with no extension on file name
      openmdao.model.newFile("newfilename", "folderpath");
      // It calls setFile so check to see if that happens as expected
      assertEquals("file/folderpath/newfilename", this.requests[2].url);
      assertEquals(this.requests[2].method, "POST" );
      assertEquals("contents=", this.requests[2].requestBody);
      this.requests[2].respond(200, { "Content-Type": "text/plain" }, '' ) ;

  },

  "test newFolder": function () {

      // Normal operation
      openmdao.model.newFolder("newfoldername", "folderpath");
      // It calls createFolder so check to see if that happens as expected
      assertEquals("file/folderpath/newfoldername", this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("isFolder=true", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '');
  },


  "test removeFile": function () {

      // Normal operation
      openmdao.model.removeFile( "filepath" );
      assertEquals("filefilepath", this.requests[0].url);
      assertEquals(this.requests[0].method, "DELETE" );
      assertEquals("file=filepath", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;

      // TODO: I do not see how to check the error handling at this point
      // Unless I do some mocking of the debug.warn

  },


  "test importFile": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;

      // Normal execution
      openmdao.model.importFile("filepath.py", success_handler, error_handler) ;
      assertEquals("command", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(this.requests[0].requestBody, "command=from+filepath+import+*");
      this.requests[0].respond(200, {"Content-Type": "application/json"}, '{"status" : "OK"}');

      sinon.assert.calledOnce(success_handler);
      sinon.assert.notCalled(error_handler);

  },



  "test runModel": function () {

      // Normal execution
      openmdao.model.runModel() ;
      assertEquals("exec", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(null, this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, 'OK' ) ;

  },



  "test execFile": function () {

      // Normal execution
      openmdao.model.execFile("/file\\path") ;
      assertEquals("exec", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals("filename=file%2Fpath", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, 'OK' ) ;

  }

});

