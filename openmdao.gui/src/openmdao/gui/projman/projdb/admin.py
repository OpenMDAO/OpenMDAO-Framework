from projdb.models import Project
from django.contrib import admin

class ProjectInline(admin.TabularInline):
    model = Project

class ProjectAdmin(admin.ModelAdmin):
    fieldsets = [
        (None,               {'fields': ['user','projectname', 'version', 'description', 'shared']}),
        ('File information', {'fields': ['filename'], 'classes': ['collapse']}),
    ]
    list_display = ('user','projectname','version','description','shared','modified')
    list_filter = ['user']
    search_fields = ['projectname', 'description']
    
admin.site.register(Project, ProjectAdmin)