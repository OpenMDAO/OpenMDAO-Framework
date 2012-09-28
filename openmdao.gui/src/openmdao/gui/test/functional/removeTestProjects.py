import sys

from nose.tools import with_setup


if sys.platform != 'win32':  # No testing on Windows yet.
    from .util import main, setup_server, teardown_server, generate, begin

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_new_project(browser):
    projects_page = begin(browser)
    projects_page.delete_all_test_projects()


if __name__ == '__main__':
    main()

