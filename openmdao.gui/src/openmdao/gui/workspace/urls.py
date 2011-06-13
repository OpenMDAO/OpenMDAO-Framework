from django.conf.urls.defaults import patterns, include, url
from django.views.generic import DetailView, ListView, list_detail

from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('workspace.views',
    # custom views
    (r'^$',               'Workspace'),
    (r'^components',      'Components'),
    (r'^component/(.*)',  'Component'),
    (r'^addons',          'AddOns'),
    (r'^command',         'Command'),
    (r'^exec',            'Exec'),
    (r'^exit',            'Exit'),
    (r'^file/(.*)',       'File'),
    (r'^files',           'Files'),
    (r'^logout',          'Logout'),
    (r'^model',           'Model'),    # TODO: remove?
    (r'^output',          'Output'),
    (r'^project',         'Project'),
    (r'^types',           'Types'),
    # (r'^upload',          'Upload'),
    (r'^workflow',        'Workflow'),
)
