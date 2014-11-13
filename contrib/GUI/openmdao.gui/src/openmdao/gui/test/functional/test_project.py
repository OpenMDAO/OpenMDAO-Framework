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
                 begin, random_project, edit_project, import_project, \
                 get_browser_download_location_path, broken_chrome

from pageobjects.workspace import WorkspacePage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


#Test metadata of a project:
def _test_last_saved_metadata(browser):

    def last_saved(target):
        def wrapper(projects_page, project_name, timestamp, *args, **kargs):
            workspace_page = projects_page.open_project(project_name)
            result = target(workspace_page)

            projects_page = workspace_page.close_workspace()

            if result is False:
                last_saved = timestamp
            else:
                metadata = projects_page.get_project_metadata(project_name)
                last_saved = date_to_datetime(metadata['last_saved'])
                assert(last_saved > timestamp)

            return projects_page, project_name, last_saved

        return wrapper

    def date_to_datetime(date):
        date_regex = re.compile("(\d+)-(\d+)-(\d+)")
        time_regex = re.compile("(\d+):(\d+):(\d+)")

        match_object = date_regex.search(date)
        year  = int(match_object.group(1))
        month = int(match_object.group(2))
        day   = int(match_object.group(3))

        match_object = time_regex.search(date)
        hours   = int(match_object.group(1))
        minutes = int(match_object.group(2))
        seconds = int(match_object.group(3))

        return datetime.datetime(year, month, day, hours, minutes, seconds)

    @last_saved
    def add_file(workspace_page):
        file_path = pkg_resources.resource_filename('openmdao.gui.test.functional', 'files/simple_paraboloid.py')
        workspace_page.add_file(file_path)

    @last_saved
    def new_file(workspace_page):
        workspace_page.new_file('test_file.py')

    @last_saved
    def rename_file(workspace_page):
        workspace_page.get_files()
        workspace_page.rename_file('test_file.py', 'best_file.py')

    @last_saved
    def edit_file(workspace_page):
        if broken_chrome():
            print "Skipping testing metadata after editing file due to broken chrome driver."
            return False

        workspace_window = browser.current_window_handle

        editor_page = workspace_page.open_editor()
        editor_page.edit_file('test_file.py', dclick=False)
        editor_page.add_text_to_file('#just a comment\n')
        editor_page.save_document(check=False)

        browser.switch_to_window(workspace_window)
        port = workspace_page.port
        workspace_page = WorkspacePage.verify(browser, port)

    @last_saved
    def delete_file(workspace_page):
        workspace_page.delete_file('best_file.py')

    @last_saved
    def add_object(workspace_page):
        workspace_page.add_library_item_to_dataflow("openmdao.main.assembly.Assembly", 'top')

    @last_saved
    def replace_object(workspace_page):
        workspace_page.replace_driver("top", 'SLSQPdriver')

    @last_saved
    def commit_project(workspace_page):
        top = workspace_page.get_dataflow_figure('top')
        top.remove()
        workspace_page.commit_project()

    @last_saved
    def revert_project(workspace_page):
        workspace_page.add_library_item_to_dataflow("openmdao.main.assembly.Assembly", 'top')
        workspace_page = workspace_page.revert_project()


    projects_page = begin(browser)
    projects_page, project_dict = random_project(projects_page.new_project(),
                                                 verify=True, load_workspace=False)

    project_name = project_dict['name']
    metadata = projects_page.get_project_metadata(project_name)
    created_time = date_to_datetime(metadata['created'])

    #Testing metadata for file operations
    projects_page, project_name, add_file_time       = add_file(projects_page, project_name, created_time)
    projects_page, project_name, new_file_time       = new_file(projects_page, project_name, add_file_time)
    projects_page, project_name, edit_file_time      = edit_file(projects_page, project_name, new_file_time)
    projects_page, project_name, rename_file_time    = rename_file(projects_page, project_name, edit_file_time)
    projects_page, project_name, delete_file_time    = delete_file(projects_page, project_name, rename_file_time)

    #Testing metadata for project operations
    projects_page, project_name, add_object_time     = add_object(projects_page, project_name, delete_file_time)
    projects_page, project_name, replace_object_time = replace_object(projects_page, project_name, add_object_time)
    projects_page, project_name, commit_project_time = commit_project(projects_page, project_name, replace_object_time)
    projects_page, project_name, revert_project_time = revert_project(projects_page, project_name, commit_project_time)

    projects_page.delete_project(project_name)


#Test creating a project
def _test_new_project(browser):

    browser_download_location_path = get_browser_download_location_path(browser)

    projects_page = begin(browser)
    eq(projects_page.welcome_text, 'Welcome to OpenMDAO %s' % __version__)

    # Create a project.
    projects_page, project_dict = random_project(projects_page.new_project(),
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
