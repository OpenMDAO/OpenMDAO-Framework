import glob
import os
import time
import datetime
import re
import pkg_resources 

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.main.releaseinfo import __version__

from util import main, setup_server, teardown_server, generate, \
                 begin, new_project, edit_project, import_project, \
                 get_browser_download_location_path, broken_chrome

from pageobjects.workspace import WorkspacePage

@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


#Test metadata of a project:
def _test_last_saved_metadata(browser):
    
    def open_project(target):
        def wrapper(self, project_name, *args, **kargs):
            workspace_page, project_dict = projects_page.open_project( project_name )
            result = target(self, *args, **kargs)

            return result

        return wrapper

    def close_project(target):
        def wrapper(self, *args, **kargs):
            result = target(self, *args, **kargs)
            projects_page = workspace_page.close_workspace()

            return result

        return wrapper

    def metadata(target):
        def wrapper(self, *args, **kargs):
            result = target(*args, **kargs)
            metadata = projects_page.get_project_metadata( project_name )

            return result, metadata

    def last_saved(target):
        def wrapper( metadata, *args, **kargs )
            result = target(*args, **kargs)
            last_saved = date_to_datetime( metadata['last_saved'] )
            
            return result

        return wrapper

    def date_to_datetime( date ):
        date_regex = re.compile( "(\d+)-(\d+)-(\d+)")
        time_regex = re.compile( "(\d+):(\d+):(\d+)" )

        match_object = date_regex.search( date )
        year  = int( match_object.group(1) )
        month = int( match_object.group(2) )
        day   = int( match_object.group(3) )

        match_object = time_regex.search( date )
        hours   = int( match_object.group(1) )
        minutes = int( match_object.group(2) )
        seconds = int( match_object.group(3) )

        return datetime.datetime( year, month, day, hours, minutes, seconds  )
  
    def add_file(workspace_page):
        file_path = pkg_resources.resource_filename('openmdao.gui.test.functional', 'files/simple_paraboloid.py')
        workspace_page.add_file( file_path )

    def new_file(workspace_page):
        workspace_page.new_file( 'test_file.py' )

    def add_object(workspace_page):
        workspace_page.add_library_item_to_dataflow("simple_paraboloid.SimpleParaboloid", 'top')
        
    def rename_file(workspace_page):
        file_names = workspace_page.get_files()
        workspace_page.rename_file('test_file.py', 'best_file.py')

    def edit_file(workspace_page):
        if broken_chrome():
            raise SkipTest('Test broken for chrome/selenium combination')

        workspace_window = browser.current_window_handle

        editor_page = workspace_page.open_editor()
        editor_page.edit_file('test_file.py', dclick=False)
        editor_page.add_text_to_file('#just a comment\n')
        editor_page.save_document(check=False)

        browser.switch_to_window(workspace_window)
        port = workspace_page.port
        workspace_page = WorkspacePage.verify(browser, port)

    def delete_file(workspace_page):
        workspace_page.delete_file('best_file.py')
        
     
    projects_page = begin(browser)
    workspace_page, project_dict = new_project(projects_page.new_project(),
                                              verify=True, load_workspace=True)

    add_file(workspace_page)
    projects_page = workspace_page.close_workspace()
    metadata = projects_page.get_project_metadata( project_dict['name'] )

    created_time = date_to_datetime( metadata['created'] )
    add_file_time = date_to_datetime( metadata['last_saved'] )

    assert( add_file_time > created_time )

    workspace_page = projects_page.open_project( project_dict['name'] )
    new_file(workspace_page)
    projects_page = workspace_page.close_workspace()
    metadata = projects_page.get_project_metadata( project_dict['name'] )

    new_file_time = date_to_datetime( metadata['last_saved'] )

    assert( new_file_time > add_file_time )

    workspace_page = projects_page.open_project( project_dict['name'] )
    add_object(workspace_page)
    projects_page = workspace_page.close_workspace()
    metadata = projects_page.get_project_metadata( project_dict['name'] )

    add_object_time = date_to_datetime( metadata['last_saved'] )
    
    assert( add_object_time > new_file_time )

    workspace_page = projects_page.open_project( project_dict['name'] )
    edit_file(workspace_page)
    projects_page = workspace_page.close_workspace()
    metadata = projects_page.get_project_metadata( project_dict['name'] )

    edit_file_time = date_to_datetime( metadata['last_saved'] )

    assert( edit_file_time > add_object_time )

    workspace_page = projects_page.open_project( project_dict['name'] )
    rename_file(workspace_page)
    projects_page = workspace_page.close_workspace()
    metadata = projects_page.get_project_metadata( project_dict['name'] )

    rename_file_time = date_to_datetime( metadata['last_saved'] )
    
    assert( rename_file_time > edit_file_time )

    workspace_page = projects_page.open_project( project_dict['name'] )
    delete_file(workspace_page)
    projects_page = workspace_page.close_workspace()

    metadata = projects_page.get_project_metadata( project_dict['name'] )

    delete_file_time = date_to_datetime( metadata['last_saved'] )
    
    assert( delete_file_time > rename_file_time )

    projects_page.logout()

    
#Test creating a project
def _test_new_project(browser):

    browser_download_location_path = get_browser_download_location_path(browser)

    projects_page = begin(browser)
    eq(projects_page.welcome_text, 'Welcome to OpenMDAO %s' % __version__)

    # Create a project.
    projects_page, project_dict = new_project(projects_page.new_project(),
                                              verify=True, load_workspace=False)
    # Go back to projects page to see if it is on the list.
    assert projects_page.contains(project_dict['name'])

    # Make sure all the project meta data was saved correctly.
    edit_dialog = projects_page.edit_project(project_dict['name'])
    eq(str(edit_dialog.project_name).strip(), project_dict['name'])
    eq(str(edit_dialog.description).strip(), project_dict['description'])
    eq(str(edit_dialog.version).strip(), project_dict['version'])  # maxlength

    # Edit the project meta data
    project_dict['description'] = "pony express"
    project_dict['version'] = "23432"

    projects_page = edit_project(edit_dialog,
                         project_dict['name'],
                         project_dict['description'],
                         project_dict['version'])

    # Make sure all the new project meta data was saved correctly.
    edit_dialog = projects_page.edit_project(project_dict['name'])
    eq(str(edit_dialog.project_name).strip(), project_dict['name'])
    eq(str(edit_dialog.description).strip(), project_dict['description'])
    eq(str(edit_dialog.version).strip(), project_dict['version'][:5])  # maxlength
    edit_dialog.cancel()

    # Export the project
    projects_page.export_project(project_dict['name'])
    project_pattern = project_dict['name'].replace(' ', '_') + '-*.proj'
    project_pattern = os.path.join(browser_download_location_path,
                                   project_pattern)
    for i in range(10):  # Give the download time to complete.
        time.sleep(1)
        project_path = glob.glob(project_pattern)
        if project_path:
            break
    else:
        assert False, 'Download of %r timed-out' % project_pattern
    assert len(project_path) == 1
    project_path = project_path[0]

    # Delete the project in preparation for reimporting
    projects_page.delete_project(project_dict['name'])

    # Make sure the project was deleted
    assert not projects_page.contains(project_dict['name'], False)

    # Import the project and give it a new name
    projects_page, project_dict = import_project(projects_page.import_project(),
                                                 project_path, verify=True,
                                                 load_workspace=False)
    # Go back to projects page to see if it is on the list.
    assert projects_page.contains(project_dict['name'])

    # Delete the project that was just imported
    projects_page.delete_project(project_dict['name'])

    # remove the downloaded file
    os.remove(project_path)


if __name__ == '__main__':
    main()
