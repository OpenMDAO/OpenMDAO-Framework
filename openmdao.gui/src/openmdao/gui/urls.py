from django.conf.urls.defaults import patterns, include, url
from django.contrib.auth import views as authviews

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    # favicon
    (r'^favicon\.ico$', 'django.views.generic.simple.redirect_to', {'url': '/static/images/favicon.ico'}),
    
    # default to projdb app
    (r'^$',         'projdb.views.index'),
    (r'^login/$',   'workspace.views.Exit'),  # Workspace Logout menu item currently currently points to /login
    
    # projects
    (r'^projects/', include('projdb.urls')),
    
    # workspace
    (r'^workspace/', include('workspace.urls')),
    
    # registration view is in projdb at the moment
    (r'^accounts/register/$', 'projdb.views.register'),
    
    # authentication
    (r'^accounts/login/$',              'django.contrib.auth.views.login', {'template_name': 'login.html'}),
    (r'^accounts/logout/$',             'django.contrib.auth.views.logout_then_login'),
    (r'^accounts/password_change/$',    'django.contrib.auth.views.password_change'),
    (r'^accounts/password_reset/$',     'django.contrib.auth.views.password_reset'),
    (r'^accounts/password_reset/done/$', 'django.contrib.auth.views.password_reset_done'),
    
    # admin
    (r'^admin/', include(admin.site.urls)),
    (r'^admin/doc/', include('django.contrib.admindocs.urls')),
)
