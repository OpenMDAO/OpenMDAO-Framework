from django.conf.urls.defaults import patterns, include, url
from django.views.generic import DetailView, ListView, list_detail

from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('projdb.views',
    # custom views
    (r'^$', 'index'),
    (r'^(?P<project_id>\d+)/$', 'detail'),
    
    # debugging views
    (r'^meta$', 'display_meta'),
)
