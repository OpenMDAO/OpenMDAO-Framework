from django.conf.urls.defaults import patterns, include, url
from django.views.generic import DetailView, ListView, list_detail

from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('workspace.views',
    # custom views
    (r'^$',                 'Workspace'),
    (r'^components',        'Components'),
    (r'^component/(.*)',    'Component'),
    (r'^connections/(.*)',  'Connections'),
    (r'^addons',            'AddOns'),
    (r'^close',             'Close'),
    (r'^command',           'Command'),
    (r'^dataflow/(.*)',     'Dataflow'),
    (r'^exec',              'Exec'),
    (r'^exit',              'Exit'),
    (r'^file/(.*)',         'File'),
    (r'^files',             'Files'),
    (r'^geometry',          'Geometry'),
    (r'^logout',            'Logout'),
    (r'^model',             'Model'),
    (r'^output',            'Output'),
    (r'^project',           'Project'),
    (r'^top',               'Top'),
    (r'^types',             'Types'),
    (r'^upload',            'Upload'),
    (r'^workflow/(.*)',     'Workflow'),
    
    # a test view
    (r'^test',              'Test'),
)
