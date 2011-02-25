
import sys
import os
import webbrowser

_allowed_plugin_types = set([ 'openmdao.component',
                              'openmdao.driver',
                              'openmdao.variable',
                              'openmdao.caseiterator',
                              'openmdao.caserecorder',
                              'openmdao.resource_allocator'
                              ])


def plugin(*plugin_types):
    """A class decorator that registers a class as an OpenMDAO plugin."""
    def _reg_plugin(c):
        for typ in plugin_types:
            if typ not in _allowed_plugin_types:
                raise NameError("'%s' is not a valid OpenMDAO plugin group name" % typ)
            # at some point, actually register the class as a plugin here...
        return c
    return _reg_plugin


def _plugin_docs(argv=None):
    """A command line script (plugin_docs) points to this. It brings up
    the sphinx documentation for the named plugin in a browser.
    """
    if argv is None:
        argv = sys.argv[1:]
        
    if len(argv) != 1:
        print 'usage: plugin_docs <plugin_name>'
        sys.exit(-1)
        
    if len(argv) > 1:
        browser = argv[1]
    else:
        browser = None
        
    plugin_docs(argv[0], browser)
        
        
def plugin_docs(plugin_name, browser=None):
    """This brings up the sphinx docs for the named plugin using the
    specified browser.  The plugin must be importable in the current 
    environment.
    
    plugin_name: str
        Name of the plugin.
        
    browser: str (optional)
        Name of the browser (according to the webbrowser library) to
        use to view the plugin docs.  If none is specified, the platform
        default browser will be used.
    """
    try:
        mod = __import__(plugin_name)
    except ImportError:
        raise RuntimeError("Can't locate plugin '%s'" % plugin_name)
    
    idx = os.path.join(os.path.dirname(os.path.abspath(mod.__file__)),
                       'sphinx_build', 'html', 'index.html')
    
    if not os.path.isfile(idx):
        raise RuntimeError("Cannot locate index file for plugin '%s'" % plugin_name)
    
    wb = webbrowser.get(browser)
    wb.open(idx)

