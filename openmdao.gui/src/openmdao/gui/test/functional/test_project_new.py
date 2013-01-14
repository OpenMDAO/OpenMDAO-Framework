import sys

from nose.tools import eq_ as eq
from nose.tools import with_setup

from util_new import main, setup_server, teardown_server, generate, \
                 begin, new_project, edit_project

@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser

#Test creating a project
def _test_new_project(browser):
    projects_page = begin(browser)

    # Create a project.
    projects_page, project_dict = new_project(projects_page.new_project(), verify=True, load_workspace=False)
    # Go back to projects page to see if it is on the list.
    assert projects_page.contains(project_dict['name'])
    
    #Make sure all the project meta data was saved correctly.
    edit_dialog = projects_page.edit_project(project_dict['name'])
    eq(str(edit_dialog.project_name).strip(), project_dict['name'])
    eq(str(edit_dialog.description).strip(), project_dict['description'])
    eq(str(edit_dialog.version).strip(), project_dict['version'][:5])  # maxlength

    #Edit the project meta data
    project_dict['description'] = "pony express"
    project_dict['version'] = "23432"

    projects_page = edit_project(edit_dialog,
                         project_dict['name'],
                         project_dict['description'],
                         project_dict['version'],
                         load_workspace=False)

    #Make sure all the new project meta data was saved correctly.
    edit_dialog = projects_page.edit_project(project_dict['name'])
    eq(str(edit_dialog.project_name).strip(), project_dict['name'])
    eq(str(edit_dialog.description).strip(), project_dict['description'])
    eq(str(edit_dialog.version).strip(), project_dict['version'][:5])  # maxlength
    edit_dialog.cancel()

    #Export the project
    projects_page.export_project(project_dict['name'])

    #Delete the project in preparation for reimporting
    projects_page.delete_project(project_dict['name'])

    #Import the project
    #projects_page.

#Test editing a project
#def _edit_new_project(browser):
#    projects_page = begin(browser)
#
#    #Create a project
#    projects_page, project_dict = new_project(projects_page.new_project(), verify=False)
#    
#    # Edit the project meta data


if __name__ == '__main__':
    main()
