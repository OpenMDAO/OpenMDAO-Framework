from django.conf.urls.defaults import patterns, include, url
from django.views.generic import DetailView, ListView, list_detail

from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('workspace.views',
    # custom views
    (r'^$',               'Workspace'),
    (r'^component/(.*)',  'Component'),
    # (r'^addons',          'AddOns'),
    (r'^command',         'Command'),
    (r'^exec',            'Exec'),
    (r'^exit',            'Exit'),
    (r'^file/(.*)',       'File'),
    (r'^files',           'Files'),
    (r'^cwd',             'CWD'),
    (r'^login',           'Exit'),
    (r'^model',           'Model'),
    (r'^output',          'Output'),
    (r'^types',           'Types'),
    # (r'^upload',          'Upload'),
)
