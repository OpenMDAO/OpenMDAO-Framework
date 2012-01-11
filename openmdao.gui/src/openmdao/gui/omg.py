import os, sys
from optparse import OptionParser
from openmdao.util.network import get_unused_ip_port
from openmdao.gui.util import ensure_dir, launch_browser

def init(reset):
    ''' first time setup (or re-setup)
    '''
    print "Initializing settings..."
    
    user_dir = os.path.expanduser("~/.openmdao/gui/")  # TODO: could put in a prefs file
    ensure_dir(user_dir)
    
    settings_file = "settings.py"
    database_file = user_dir+"mdaoproj.db"
    media_storage = user_dir+"media"
    
    if os.path.exists(settings_file):
        os.remove(settings_file)
    o = open(settings_file,"a") #open for append
    for line in open("settings.tmp"):
       line = line.replace("'NAME': 'mdaoproj.db'","'NAME': '"+database_file+"'")
       line = line.replace("MEDIA_ROOT = ''","MEDIA_ROOT = '"+media_storage+"'")
       o.write(line) 
    o.close()
    
    import settings
    print "MEDIA_ROOT=",settings.MEDIA_ROOT
    print "DATABASE=",settings.DATABASES['default']['NAME']
    
    print "Resetting project database..."
    if reset and os.path.exists(database_file):
        print "Deleting existing project database..."
        os.remove(database_file)
    from django.core.management import execute_manager
    execute_manager(settings,argv=[__file__,'syncdb'])

def dev(port):
    ''' run django development server
    '''
    import settings
    from django.core.management import execute_manager
    execute_manager(settings,argv=[__file__,'runserver',str(port)])

def pro(port):
    ''' run cherrypy 'production' server
    '''
    print "Running server on port",str(port)
    print "Quit the server with CTRL-BREAK"
    
    import django.core.handlers.wsgi
    from web.httpserver import WSGIServer, StaticMiddleware, LogMiddleware
    
    os.environ['DJANGO_SETTINGS_MODULE'] = 'settings'
    func = django.core.handlers.wsgi.WSGIHandler()
    func = StaticMiddleware(func)
    func = LogMiddleware(func)

    server = WSGIServer(('localhost', port), func)
    server.timeout = 50

    try:
        server.start()
    except KeyboardInterrupt:
        server.stop()

def get_parser():
    ''' create a parser for command line arguments
    '''
    parser = OptionParser()
    parser.add_option("-d", "--dev", action="store_true", dest="devserver",
                      help="use Django development server")
    parser.add_option("-p", "--port", type="int", dest="port", default=0,
                      help="port to run server on (defaults to any available port)")
    parser.add_option("-b", "--browser", dest="browser", default="chrome",
                      help="preferred browser")
    parser.add_option("-s", "--server", action="store_true", dest="serveronly",
                      help="don't launch browser, just run server")
    parser.add_option("-i", "--init", action="store_true", dest="initialize",
                      help="(re)initialize settings")
    parser.add_option("-r", "--reset", action="store_true", dest="reset",
                      help="reset project database (valid only with -i and without -d)")
    return parser

def main():
    ''' process command line arguments and do as commanded
    '''
    parser = get_parser()
    (options, args) = parser.parse_args()
    
    if options.initialize or not os.path.exists('settings.py'):
        if options.reset and not options.devserver:
            init(reset=True)
        else:
            init(reset=False)
        
    if (options.port < 1):
        options.port = get_unused_ip_port()    

    if not options.serveronly:
        # NOTE: with dev server, you are going to get two browsers
        #       this is due to forking I suppose
        #       not sure what can be done about it
        launch_browser(options.port, options.browser)

    if (options.devserver):
        dev(options.port)
    else:
        pro(options.port)


if __name__ == '__main__':
    main()
