
if __name__ == "__main__":
    import os
    import django.core.handlers.wsgi
    import web.httpserver

    os.environ['DJANGO_SETTINGS_MODULE'] = 'settings'
    app = django.core.handlers.wsgi.WSGIHandler()

    # runsimple adds middleware for static files & logging 
    web.httpserver.runsimple(app,server_address=("127.0.0.1", 8000))
