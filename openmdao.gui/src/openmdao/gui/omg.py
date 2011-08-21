import os, sys
from optparse import OptionParser
from mdao_util import PickUnusedPort, launch_browser

def dev(port):
    import settings
    from django.core.management import execute_manager
    execute_manager(settings,argv=[__file__,'runserver',str(port)])

def pro(port):
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

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-d", "--dev", action="store_true", dest="devserver",
                      help="use Django development server")
    parser.add_option("-p", "--port", type="int", dest="port", default=0,
                      help="port to run server on (defaults to any available port)")
    parser.add_option("-b", "--browser", dest="browser", default="chrome",
                      help="preferred browser (defaults to firefox)")
    parser.add_option("-s", "--server", action="store_true", dest="serveronly",
                      help="don't launch browser, just run server")

    (options, args) = parser.parse_args()
    
    if (options.port < 1):
        options.port = PickUnusedPort()    

    if not options.serveronly:
        # NOTE: with dev server, you are going to get two browsers
        #       this is due to forking I suppose
        #       not sure what can be done about it
        launch_browser(options.port, options.browser)

    if (options.devserver):
        dev(options.port)
    else:
        pro(options.port)

