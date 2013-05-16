import glob
import os
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.main.releaseinfo import __version__

from util import main, setup_server, teardown_server, generate, \
                 begin, new_project, edit_project, import_project, \
                 get_browser_download_location_path


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


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
