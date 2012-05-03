from nose.tools import eq_ as eq
from nose.tools import with_setup

from util import setup_server, teardown_server, generate, \
                 begin, dbug, new_project


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_new_project(browser):
    dbug(browser, '_test_new_project')
    projects_page = begin(browser)

    # Create a project.
    dbug(browser, '_test_new_project: create')
    project_info_page, project_dict = new_project(projects_page.new_project())

    # Go back to projects page to see if it is on the list.
    dbug(browser, '_test_new_project: verify')
    projects_page = project_info_page.go_to_projects_page()
    assert projects_page.contains(project_dict['name'])
    
    # Make sure all the project meta data was saved correctly.
    dbug(browser, '_test_new_project: edit')
    project_info_page = projects_page.edit_project(project_dict['name'])
    eq( project_info_page.project_name, project_dict['name'] )
    eq( project_info_page.description, project_dict['description'] )
    eq( project_info_page.version, project_dict['version'][:5] )  # maxlength

    # Clean up.
    dbug(browser, '_test_new_project: delete')
    project_info_page.delete_project()
    dbug(browser, '_test_new_project: done')


if __name__ == '__main__':
    if True:
        # Run under nose.
        import sys
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        nose.runmodule()
    else:
        # Run outside of nose.
        from util import setup_chrome, setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_new_project(browser)
        teardown_server()

