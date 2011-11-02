from django.conf.urls.defaults import patterns, include, url
from django.views.generic import DetailView, ListView, list_detail

from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('projdb.views',
    # custom views
    (r'^$', 'index'),
    (r'^(?P<project_id>\d+)/$', 'detail'),
    (r'^new/$', 'new'),
    (r'^add/$', 'add'),
    (r'^delete/(?P<project_id>\d+)/$', 'delete'),
    (r'^download/(?P<project_id>\d+)/$', 'download'),
    
    # debugging views
    (r'^meta$', 'display_meta'),
)
