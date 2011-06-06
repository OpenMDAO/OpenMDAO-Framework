import os
import django.core.handlers.wsgi
from web.httpserver import WSGIServer, StaticMiddleware, LogMiddleware
from mdao_util import PickUnusedPort, launch_browser

if __name__ == '__main__':
    os.environ['DJANGO_SETTINGS_MODULE'] = 'settings'
    func = django.core.handlers.wsgi.WSGIHandler()
    func = StaticMiddleware(func)
    func = LogMiddleware(func)

    port = PickUnusedPort()    
    launch_browser(port)
    
    server = WSGIServer(('localhost', port), func)
    server.timeout = 50

    try:
        server.start()
    except KeyboardInterrupt:
        server.stop()
