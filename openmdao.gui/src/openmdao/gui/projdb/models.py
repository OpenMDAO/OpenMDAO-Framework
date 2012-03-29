from django.db import models 
from django.contrib.auth.models import User

def get_upload_path(instance,filename):
    print instance.user
    return str(instance.user)+'/'+filename

# Project
class Project(models.Model):
    user        = models.ForeignKey(User)
    projectname = models.CharField('project name',max_length=40)
    version     = models.CharField(max_length=40)
    description = models.CharField(max_length=200)
    modified    = models.DateTimeField('date modified', auto_now=True)
    filename    = models.FileField(max_length=200,upload_to=get_upload_path)
    shared      = models.BooleanField()
    active      = models.BooleanField()

    def __unicode__(self):
        return self.projectname

    class Meta:
        ordering = ['-modified']

