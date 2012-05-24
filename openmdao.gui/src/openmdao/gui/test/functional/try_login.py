from nose.tools import eq_ as eq
from nose.tools import with_setup

from util import TEST_CONFIG, setup_server, teardown_server, generate

from pageobjects.login import LoginPage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_successful_login(browser):
    login_page = LoginPage(browser, TEST_CONFIG['port'])
    login_page.go_to()
    eq( "Login", login_page.page_title )
    projects_page = login_page.login_successfully("herb", "herb" )
    eq( "Projects", projects_page.page_title )
    eq( "http://localhost:%d/" % TEST_CONFIG['port'], projects_page.page_url )
    assert projects_page('logout').is_present()
    login_page_again = projects_page.logout()
    eq( "Login", login_page_again.page_title )
    

def _test_unsuccessful_login(browser):
    login_page = LoginPage(browser, TEST_CONFIG['port'])
    login_page.go_to()
    eq( "Login", login_page.page_title )
    new_login_page = login_page.login_unsuccessfully("herb", "notherb" )
    eq( "Login", new_login_page.page_title )


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
        from util import setup_firefox
        setup_server(virtual_display=False)
        browser = setup_firefox()
        _test_successful_login(browser)
        _test_unsuccessful_login(browser)
        teardown_server()

