import sys

from nose.tools import eq_ as eq
from nose.tools import with_setup


if sys.platform != 'win32':  # No testing on Windows yet.
    from util import setup_server, teardown_server, generate, begin, new_project

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_new_project(browser):
    projects_page = begin(browser)

    # Create a project.
    project_info_page, project_dict = new_project(projects_page.new_project())

    # Go back to projects page to see if it is on the list.
    projects_page = project_info_page.go_to_projects_page()
    assert projects_page.contains(project_dict['name'])
    
    # Make sure all the project meta data was saved correctly.
    project_info_page = projects_page.edit_project(project_dict['name'])
    eq( project_info_page.project_name, project_dict['name'] )
    eq( project_info_page.description, project_dict['description'] )
    eq( project_info_page.version, project_dict['version'][:5] )  # maxlength

    # Clean up.
    project_info_page.delete_project()


if __name__ == '__main__':
    if '--nonose' in sys.argv:
        # Run outside of nose.
        from util import setup_chrome, setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_new_project(browser)
        browser.quit()
        teardown_server()
    else:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())

