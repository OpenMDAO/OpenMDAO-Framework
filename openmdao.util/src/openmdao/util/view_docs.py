import sys

def view_docs(browser=None):
    """A script (openmdao_docs) points to this. It just pops up a browser to 
    view the openmdao sphinx docs. If the docs are not already built, it
    builds them before viewing, but if the docs already exist, it's not smart enough
    to rebuild them if they've changed since the last build.
    
    If this is run from a non-developer install, i.e., there is no local copy of
    the docs, it just looks for the docs on the openmdao.org website.
    """
    if not browser:
        for arg in sys.argv:
            if arg.startswith('--browser='):
                browser = arg.split('=')[-1].strip()
    try:
        import openmdao.devtools.build_docs
    except ImportError:
        # look for docs online
        version = openmdao.util.releaseinfo.__version__
        idxpath = 'http://openmdao.org/downloads/%s/docs' % version
        
        import webbrowser
        wb = webbrowser.get(browser)
        wb.open(idxpath)
    else:
        openmdao.devtools.build_docs.view_docs(browser)


