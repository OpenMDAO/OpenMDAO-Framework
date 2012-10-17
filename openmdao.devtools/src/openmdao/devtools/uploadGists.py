import sys, os
import re

import json
import requests

from openmdao.util.fileutil import get_ancestor_dir

#whitelist of filetypes to upload
allowFiletype = ['.py', '.csv', '.f', '.c']
#blacklist of files to ignore, even if filetype is valid
ignoreFilename = ['__init__', 'releaseinfo', 'docs-link']

#url of the github api
apiURL = 'https://api.github.com/'
#default username to upload with (provided by user)
username = "OpenMDAO-gists"
#password to log in with (will be provided by user)
password = ""

#should we notify the cookbook?
updateCookbook = True
#url of the cookbook to notify MUST END IN WITH '/'  
cookbookURL = 'http://openmdao.org/wordpress_NEWURL/cookbook/'


#directory of tutorials
tutorialsDir = os.path.join(get_ancestor_dir(__file__,5), "examples")


def uploadGists (OpenMDAO_version):
    #first of all, ask for username/password for github
    if getUsernamePass():
        return upload(OpenMDAO_version)
    else:
        return False

def upload (OpenMDAO_version):

#    """
#    file structure of tutorials:
#
#    examples
#            |_  openmdao.examples.TUTORIAL_NAME
#
#
#            |_
#                openmdao.examples.TUTORIAL_NAME
#                |_
#                    garbage we dont' care about
#
#                | MANIFEST.in (not always present. Holds file extensions we want???)
#
#                |_  
#                    openmdao 
#                    |_
#                        examples
#                        |_
#                            TUTORIAL_NAME
#                            |_
#                                test 
#                                |_  build tests for the tutorials (dont' want)
#
#                            | releaseinfo.py (dont' want)
#                            | releaseinfo.pyc (dont' want)
#                            | __init__.py (dont' want)
#
#                            |ACTUAL DOCUMENTS REQUIRED FOR THE TUTORIAL
#
#                            |something.csv (WANT)
#                            |stuff.py (WANT)
#                            |things.f (WANT)
#                            |junk.c    (WANT)
#                             
#    """

    #stores list of gist_ids to notify the cookbook about
    gist_id_list = {}

    #gets list of tutorials already on github
    existingTutorials = getListOfExistingTutorials()


    for tutorial in os.listdir(tutorialsDir):
        if os.path.isdir(os.path.join(tutorialsDir, tutorial)):
            tutorialName = tutorial.split(".")[2]
            if(tutorialName):
                directory = os.path.join(tutorialsDir, tutorial, "openmdao", "examples", tutorialName)
                files = getUploadFiles(directory, tutorialName)

                gist_id = [item['id'] for item in existingTutorials if item['name'] == tutorialName]

                if(gist_id):
                    updateGist(tutorialName, gist_id[0], directory, files, OpenMDAO_version)
                    gist_id = gist_id[0]
                else:
                    gist_id = createGist(tutorialName, directory, files, OpenMDAO_version)

                if(gist_id > -1):
                    gist_id_list[gist_id] = tutorialName


    if(updateCookbook):
        print ""
        print "Notifying Cookbook with gist #:"
        count = 0;
        for gist_id in gist_id_list:
            if not notifyCookbook(gist_id, gist_id_list[gist_id], OpenMDAO_version):
                return True
            count = count + 1
        print "Done"
        print "Updated " + str(count) + " tutorials"

    return True
    
def getUsernamePass():

    """returns successfull authorization"""

    print "Default username to upload gists to github:"
    print username

    useUsername = ""
    while useUsername.lower() not in ["y", "yes", "n", "no"]:
        useUsername = raw_input('Use default username? (y/n)')

    if useUsername.lower() in ["n", "no"]:
        getUsername()

    print "Using username: " + username
    print ""

    goodPass = getPass()

    while( not goodPass):
        newUsername = ""
        while newUsername not in ["y", "yes", "n", "no"]:
            newUsername = raw_input("Use different username? (y/n) ('n' will abort gist upload) ")
        if newUsername in ["y", "yes"]:
            getUsername()
            goodPass = getPass()
        else:
            return False

    return True
    

def getUsername():

    global username

    useUsername = "n"
    while useUsername.lower() in ['n', 'no']:
            username = raw_input("New Username: ")
            useUsername = ""
            while useUsername not in ["y", "yes", "n", "no"]:
                useUsername = raw_input("Use " + username + " to upload gists? (y/n)")


def getPass():
    """returns boolean successfull password"""
    global password

    password = raw_input('Input password for ' + username + ' on github:')
    authSuccess = authOK()

    while not authSuccess:
        print "Authorization failed for " + username

        newPass = ""
        while newPass.lower() not in ['y', 'yes', 'n', 'no']:
            newPass = raw_input("Enter a new password? (y/n)")

        if newPass.lower() in ['y', 'yes']:
            password = raw_input('Input password for ' + username + ' on github:')
            authSuccess = authOK()
        else:
            return False

    print "Authorization Success!"
    return True


def authOK():
    """ Test username/password pair on GitHub"""
    r = requests.get(apiURL + "users/" + username + "/gists", auth=(username, password))
    return r.status_code == 200 or r.status_code == 201


def getUploadFiles(directory, tutorialName):
    """
    Returns a list of files to upload to the tutorial gist
    """

    "filter out the files that we dont' want, retun the goodies"
    return filter(shouldUpload, os.listdir(directory))



def shouldUpload(f):

    "Returns if the file should be uploaded to the gist"
    filename, extension = os.path.splitext(f)

    return extension in allowFiletype and not (filename in ignoreFilename)

def makeFileDict(directory, files, file_dict):
    #create the files dict
    for f in files:
        s = open(os.path.join(directory, f),'rU').read()
        file_dict[f] = {"content": s}

    return file_dict

def makeDesc(name, OpenMDAO_version, directory=None):
    desc = "Example:"+name+": As included in OpenMDAO " + OpenMDAO_version
    if directory:
        files = os.listdir(directory)
        for f in files:
            filename, extension = os.path.splitext(f)
            if filename == 'docs-link' and extension == '.txt':
                link = open(os.path.join(directory, f), 'rU').read().strip()
                if link.startswith("http://openmdao.org"):
                    desc = desc + "<a href=\"" + link + "\"> Related Tutorial</a>"

    return desc



def updateGist(name, gist_id, directory, files, OpenMDAO_version):
    """
    Update any existing files, add new ones, and delete files that no longer exist.

    Basically: rewrite the gist, but keep the same ID.
    """

    print "Updating gist #" + gist_id + ": " + name

    #also be sure to delete files that are no longer present

    #get current state of the gist
    r = requests.get(apiURL + 'gists/' + gist_id, auth=(username, password))

    oldFiles = json.loads(r.text)['files']

    file_dict = {}

    for f in oldFiles:
        #attempt to remove all old files. Any new file we send will ignore this
        file_dict[f] = None


    desc = makeDesc(name, OpenMDAO_version, directory)
    #overwrite updatable files that would be deleted, add new files
    file_dict = makeFileDict(directory, files, file_dict)

    content = {
    "description" : desc,
    "files" : file_dict
    }

    headers = {'content-type': 'application/json'}

    r = requests.patch(apiURL + 'gists/' + gist_id, data=json.dumps(content), auth=(username, password), headers=headers)





def createGist(name, directory, files, OpenMDAO_version):

    """
    Posts a new gist to GitHub on behalf of 'username' field.

    Notify Cookbook of our existance.
    """

    print"Uploading " + name + '...',

    #create the description of the gist
    desc = makeDesc(name, OpenMDAO_version, directory)
    file_dict = makeFileDict(directory, files, {})

    #content is the actual stuff we are giving to github
    content = {
    "description": desc,
    "public": True,
    "files": file_dict}    

    headers = {'content-type': 'application/json'}

    r = requests.post(apiURL + 'gists', data=json.dumps(content), auth=(username, password), headers=headers)

    if (r.status_code != 201):
        print ""
        print r
        print "Error uploading to Github. Aborting, trying another tutorial:"
        return -1

    gist_id = json.loads(r.text)['id']
    print "Done. Gist ID = " + gist_id
    return gist_id

def notifyCookbook(gist_id, tutorialName, OpenMDAO_version):
    """
    Notify openmdao.org's cookbook that we have some new gists for it to look at!
    """

    print gist_id

    r = requests.get(cookbookURL + '?gist_id=' + gist_id)
    if not handleCookbookError(r):
        return False

    #add the tags 'openmdao-tutorial' and the name of the tutorial
    #r = requests.get(cookbookURL + '?newTagName=openmdao-tutorial%20' + \
     #   tutorialName + '%20' + 'OpenMDAO' + OpenMDAO_version + '&newTagGistID=' + gist_id )

    r = requests.get(cookbookURL + '?newTagName='\
        + tutorialName + '%20' + 'openmdao-tutorial' + '&newTagGistID=' + gist_id )
    if not handleCookbookError(r):
        return False

    return True


def handleCookbookError(r):
    """Deals with errors when updating cookbook"""
    if(r.status_code == 404):
        print ""
        print "Error Notifying Cookbook of new Gist. Is this URL correct?"
        print cookbookURL
        print ""
        print "If not, update cookbookURL in uploadGists.py in devtools to correct URL"
        print "or set updateCookbook to False in uploadGists.py in devtools"
        print ""
        print "You man run 'release finalize' with --tutorials option to"
        print "attempt to repeat this opperation at a later date"
        print "or with the --nogists option to exclude this operation"
        print ""
        raw_input("Press any Key to continue...")
        return False
    return True


def getListOfExistingTutorials():

    existingTutorials = [] #holds a list of tutorials already on github

    #begin by loading the current gists

    r = requests.get(apiURL + "users/" + username + "/gists", auth=(username, password))

    #returns a list of dictinoaries, one for each gist
    data = json.loads(r.text)

    for item in data:
        #item is a dictionary
        """
        item['files']
            files
                 files[<file key> (usually filename)]
                                [raw_url]
                                [size]
                                [type]
                                [language]
                                [filename]
            description
            url 
            created_at
            updated_at
            comments
            html_url
            id 
            user 
            git_pull_url 
            git_push_url
            public
        """
        #match the description to the name of an example, if exists. 
            #store in a list of dicts
        result = re.search('Example:(.*?):', item['description'])
        if(result):
            existingTutorials.append({'id' : item['id'], 'name': result.groups()[0]})

    return existingTutorials


