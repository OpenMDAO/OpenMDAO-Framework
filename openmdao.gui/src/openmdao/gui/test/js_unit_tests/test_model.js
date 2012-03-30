/* Test the functions in Model.js */
/* Uses the Sinon library http://sinonjs.org/ */


TestCase("ModelTest", {
  setUp: function () {
    openmdao.model = new openmdao.Model();
    this.fakeXhr = sinon.useFakeXMLHttpRequest();
    var requests = this.requests = [];

    this.fakeXhr.onCreate = function (xhr) {
      requests.push(xhr);
    };
  },

  tearDown: function () {
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


  "test addListener and updateListeners": function () {

      /* Cannot really test addListener 
           by itself since there is no access
           to the callbacks list in the Model.
         So need to test it indirectly using the
           updateListeners method
      */
      callback1 = sinon.spy() ;
      callback2 = sinon.spy() ;

      openmdao.model.updateListeners() ;
      sinon.assert.notCalled( callback1 );

      openmdao.model.addListener( '', callback1 ) ;
      openmdao.model.updateListeners() ;
      sinon.assert.calledOnce( callback1 ) ;

      openmdao.model.addListener( '', callback2 ) ;
      openmdao.model.updateListeners() ;
      sinon.assert.calledOnce( callback2 ) ;
      assertEquals(callback1.callCount, 2 ) ;

  },


  "test getTypes": function () {

      var type_info = "" ;

      var success_handler = sinon.spy(
          function(info) {
              type_info = info ;
          }) ;
      var error_handler = sinon.spy(
          function(jqXHR, textStatus, errorThrown) {
          }
      ) ;

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
      openmdao.model.addListener( callback1 ) ;

      openmdao.model.newModel( );

      // Check the requests
      assertEquals("model", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(true, this.requests[0].async);
      assertEquals(1, this.requests.length);
      assertEquals(null, this.requests[0].requestBody);

      sinon.assert.notCalled( callback1 );

      // Set the response
      this.requests[0].respond(200, {}, '');

      // Check the callbacks
      sinon.assert.calledOnce( callback1 );

  },


  "test saveProject": function () {

      callback1 = sinon.spy() ;
      openmdao.model.addListener( callback1 ) ;

      openmdao.model.saveProject( );

      // Check the requests
      assertEquals("project", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(true, this.requests[0].async);
      assertEquals(1, this.requests.length);
      assertEquals(null, this.requests[0].requestBody);

      sinon.assert.notCalled( callback1 );

      // Set the response
      this.requests[0].respond(200, {}, '');

      // Check the callbacks
      sinon.assert.calledOnce( callback1 );

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
      var listener1 = sinon.spy() ;

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
      openmdao.model.addListener( listener1 ) ;
      sinon.assert.notCalled( listener1 );
      openmdao.model.addComponent("typepath","component_name",null,callback) ;
      this.requests[2].respond(200, 'response', '' ) ; // the ajax call just queues up the request
      sinon.assert.calledTwice( callback );
      sinon.assert.calledOnce( listener1 );

   // Does it handle the if openmdao.Util statement properly?
      openmdao.Util[ "$component_name" ] = function() { } ;
      openmdao.model.addComponent("driver_typepath","component_name",null,callback) ;
      sinon.assert.calledTwice( callback ); // Should not be called this time so still 2 calls

  }, 


  "test issueCommand": function () {

      var success_handler = sinon.spy() ;
      var error_handler = sinon.spy() ;
      var listener1 = sinon.spy() ;

      // Normal execution
      openmdao.model.issueCommand("command",success_handler,error_handler) ;
      assertEquals("command", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals("command=command", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "application/json" }, '{ "status" : "OK"}' ) ;
      sinon.assert.calledOnce( success_handler );
      assertEquals({ "status" : "OK"}, success_handler.args[0][0]) ;

      // Are listeners updated?
      openmdao.model.addListener( listener1 ) ;
      sinon.assert.notCalled( listener1 );
      openmdao.model.issueCommand("command",success_handler,error_handler) ;
      this.requests[1].respond(200, 'response', '' ) ; // the ajax call just queues up the request
      sinon.assert.calledTwice( success_handler );
      sinon.assert.calledOnce( listener1 );

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

      var error_handler = sinon.spy() ;
      var listener1 = sinon.spy() ;

      // Normal operation
      openmdao.model.addListener( listener1 ) ;
      openmdao.model.setFile( "filepath", "file contents", error_handler );
      assertEquals("file/filepath", this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("contents=file+contents", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.notCalled( error_handler );
      sinon.assert.calledOnce( listener1 );
             
      // Check error handler
      openmdao.model.setFile( "filepath", "file contents", error_handler );
      this.requests[1].respond(500, { "Content-Type": "text/plain" }, '');
      sinon.assert.calledOnce( error_handler );
      sinon.assert.calledOnce( listener1 );
      
      // test what happens when backslash is in filepath
      openmdao.model.setFile( "file\\path", "file contents", error_handler );
      assertEquals("file/file/path", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledTwice( listener1 );

  }, 

  "test createFolder": function () {

      var error_handler = sinon.spy() ;
      var listener1 = sinon.spy() ;

      // Normal operation
      openmdao.model.addListener( listener1 ) ;
      openmdao.model.createFolder( "folderpath", error_handler );
      assertEquals("file/folderpath", this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("isFolder=true", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.notCalled( error_handler );
      sinon.assert.calledOnce( listener1 );
             
      // Check error handler
      openmdao.model.createFolder( "folderpath", error_handler );
      this.requests[1].respond(500, { "Content-Type": "text/plain" }, '');
      sinon.assert.calledOnce( error_handler );
      sinon.assert.calledOnce( listener1 );
      
      // test what happens when backslash is in folderpath
      openmdao.model.createFolder( "folder\\path", error_handler );
      assertEquals("file/folder/path", this.requests[2].url);
      assertEquals( this.requests.length, 3 ) ;
      this.requests[2].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledTwice( listener1 );

  }, 

  "test newFile": function () {

      var listener1 = sinon.spy() ;
      var modeltest = this ; // so we can refer to it in the promptForValue stub
      modeltest.newfilename = "" ; // This is normally entered via a jQuery dialog
      

      // stub Util.promptForValue
      var stub = sinon.stub(openmdao.Util, "promptForValue", 
          function( prompt, callback ) {
              // Here "this" is openmdao.Util so we cannot
              //   use that
              callback( modeltest.newfilename );
          }
                           );

      openmdao.model.addListener( listener1 ) ;

      // Normal operation with JSON new file name
      modeltest.newfilename = "newfilename.json" ;
      openmdao.model.newFile( "folderpath" );
      // It calls setFile so check to see if that happens as expected
      assertEquals("file/folderpath/" + this.newfilename, this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("contents=%5B%5D", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledOnce( listener1 );

      // Normal operation with .py new file name
      modeltest.newfilename = "newfilename.py" ;
      openmdao.model.newFile( "folderpath" );
      // It calls setFile so check to see if that happens as expected
      assertEquals("file/folderpath/" + this.newfilename, this.requests[1].url);
      assertEquals(this.requests[1].method, "POST" );
      assertEquals("contents=%22%22%22%0A+++folderpath%2Fnewfilename.py%0A%22%22%22%0A%0A", 
           this.requests[1].requestBody);
      this.requests[1].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledTwice( listener1 );

      // Normal operation with no extension on file name
      modeltest.newfilename = "newfilename" ;
      openmdao.model.newFile( "folderpath" );
      // It calls setFile so check to see if that happens as expected
      assertEquals("file/folderpath/" + this.newfilename, this.requests[2].url);
      assertEquals(this.requests[2].method, "POST" );
      assertEquals("contents=", this.requests[2].requestBody);
      this.requests[2].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledThrice( listener1 );

      stub.restore() ;
  }, 

  "test newFolder": function () {

      var listener1 = sinon.spy() ;
      var modeltest = this ; // so we can refer to it in the promptForValue stub
      modeltest.newfoldername = "" ; // This is normally entered via a jQuery dialog
      
      // stub Util.promptForValue
      var stub = sinon.stub(openmdao.Util, "promptForValue", 
          function( prompt, callback ) {
              // Here "this" is openmdao.Util so we cannot
              //   use that
              callback( modeltest.newfoldername );
          }
                           );

      openmdao.model.addListener( listener1 ) ;

      // Normal operation
      modeltest.newfilename = "newfoldername" ;
      openmdao.model.newFolder( "folderpath" );
      // It calls createFolder so check to see if that happens as expected
      assertEquals("file/folderpath/" + this.newfoldername, this.requests[0].url);
      assertEquals(this.requests[0].method, "POST" );
      assertEquals("isFolder=true", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledOnce( listener1 );

      stub.restore() ;
  }, 


  "test uploadFile": function () {

      /* TODO: Not much to test yet since uploadFile is not complete yet */
      
      var stub = sinon.stub(openmdao.Util, "popupWindow", 
          function( url,title,h,w ) {
              /* Do nothing for now */
          }
                           );
      openmdao.model.uploadFile( );
      sinon.assert.calledOnce( stub );

      stub.restore() ;
  }, 


  "test removeFile": function () {

      var listener1 = sinon.spy() ;

      // Normal operation
      openmdao.model.addListener( listener1 ) ;
      openmdao.model.removeFile( "filepath" );
      assertEquals("filefilepath", this.requests[0].url);
      assertEquals(this.requests[0].method, "DELETE" );
      assertEquals("file=filepath", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, '' ) ;
      sinon.assert.calledOnce( listener1 );
             
      // TODO: I do not see how to check the error handling at this point
      // Unless I do some mocking of the debug.warn

  }, 


  "test importFile": function () {

      var listener1 = sinon.spy() ;

      // Normal execution
      openmdao.model.addListener( listener1 ) ;
      openmdao.model.importFile( "filepath.py" ) ;
      assertEquals("command", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(this.requests[0].requestBody, "command=from+filepath+import+*" );
      this.requests[0].respond(200, { "Content-Type": "application/json" }, '{ "status" : "OK"}' ) ;
      sinon.assert.calledOnce( listener1 );

      /* TODO: Maybe do more testing in this one. Right now the call to 
               issueCommand inside of importFile does not call callbacks */

  }, 



  "test runModel": function () {

      var listener1 = sinon.spy() ;

      // Normal execution
      // Note that runModel does an Ajax call and then it calls issueCommand
      //   which also does an Ajax call. So there are two requests/responses
      openmdao.model.addListener( listener1 ) ;
      openmdao.model.runModel() ;
      assertEquals("exec", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(null, this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, 'OK' ) ;
      this.requests[1].respond(200, { "Content-Type": "application/json" }, '{ "status" : "OK"}' ) ;
      assertEquals(this.requests[1].requestBody, 'command=print+%22OK%22' );
      sinon.assert.calledOnce( listener1 );

      /* TODO: Maybe do more testing in this one. Right now the call to 
               issueCommand inside of importFile does not call callbacks */

  }, 



  "test execFile": function () {

      var listener1 = sinon.spy() ;

      // Normal execution
      openmdao.model.addListener( listener1 ) ;
      openmdao.model.execFile("/file\\path") ;
      assertEquals("exec", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals("filename=file%2Fpath", this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, 'OK' ) ;
      sinon.assert.calledOnce( listener1 );

      /* TODO: Maybe do more testing in this one. Right now the call to 
               issueCommand inside of execFile does not call callbacks */

  }, 


  "test exit": function () {

      // Normal execution
      openmdao.model.exit() ;
      assertEquals("exit", this.requests[0].url);
      assertEquals("POST", this.requests[0].method);
      assertEquals(null, this.requests[0].requestBody);
      this.requests[0].respond(200, { "Content-Type": "text/plain" }, 'OK' ) ;

  }, 





});

