from django.conf.urls.defaults import patterns, include, url
from django.views.generic import DetailView, ListView
from projdb.models import Project

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('projdb.views',
    # custom views
    (r'^$', 'index'),
    (r'^(?P<project_id>\d+)/$', 'detail'),
    
    # generic views
    # (r'^$',
        # ListView.as_view(
            # queryset=Project.objects.order_by('-modified'),
            # template_name='projdb/index.html')),
    # (r'^(?P<pk>\d+)/$',
        # DetailView.as_view(
            # model=Project,
            # template_name='projdb/detail.html')),    
)
