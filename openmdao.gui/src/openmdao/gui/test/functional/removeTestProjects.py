from nose.tools import with_setup

from util import main, setup_server, teardown_server, generate, begin


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_projects_begone(browser):
    projects_page = begin(browser)
    projects_page.delete_projects('testing project', True)


if __name__ == '__main__':
    main()
