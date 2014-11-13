/*
 * This patch is a workaround for an uncaught exception that occasionally
 * occurs during testing with Firefox. The symptoms are the test hangs
 * and the error log shows an execption referring to this code.
 * Submitted to Selenium site, but their response was 'we know how to fix
 * it, but we need a test case to be sure it stays fixed'.
 */
DelayedCommand.prototype.shouldDelayExecutionForPendingRequest_ = function() {
  if("unstable" == loadStrategy_) {
    return!1
  }
  if(this.loadGroup_.isPending()) {
    for(var a = !1, b = 0, c = this.loadGroup_.requests;c.hasMoreElements();) {
// command_processor.js: 10491
//      var d = c.getNext().QueryInterface(Components.interfaces.nsIRequest), e = !1;
      var d = c.getNext();
      try {
        d = d.QueryInterface(Components.interfaces.nsIRequest);
      } catch (exc) {
        fxdriver.Logger.dumpn('QueryInterface(Components.interfaces.nsIRequest) failed for:');
        fxdriver.Logger.dump(d);
        return !0;
      }
      var e = !1;
// end of patch
      try {
        e = d.isPending()
      }catch(f) {
        return!0
      }
      if(e && (b += 1, a = a || "about:document-onload-blocker" == d.name, 1 < b)) {
        return!0
      }
    }
    if(b && !a) {
      return fxdriver.Logger.dumpn("Ignoring pending about:document-onload-blocker request"), !0
    }
  }
  return!1
};

