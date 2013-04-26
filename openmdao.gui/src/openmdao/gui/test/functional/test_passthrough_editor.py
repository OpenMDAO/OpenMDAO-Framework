"""
Tests of passthrough editor functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_passthrough_editor(browser):
    project_dict, workspace_page = startup(browser)

    # Import variable_editor.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/passthrough_editors.py')
    workspace_page.add_file(file_path)

    workspace_page.add_library_item_to_dataflow('passthrough_editors.Topp', "top")

    time.sleep(2)
    top = workspace_page.get_dataflow_figure("top")
    top._context_click('edit_passthroughs')

    expand_i = '//*[@id="p1"]/ins'
    browser.find_element_by_xpath(expand_i).click()
    time.sleep(2)
    y_box = '//*[@id="check_y"]'
    y_btn = browser.find_element_by_xpath(y_box)
    eq(y_btn.is_selected(), True)  # check existing passthrough

    browser.find_element_by_xpath('//*[@id="y"]/a').click()  # remove passthrough
    time.sleep(1)

    workspace_page.do_command("top.list_connections()")
    time.sleep(.5)
    output = workspace_page.history.split("\n")[-1]
    eq("('y', 'p1.y')" in output, False)  # verify removed passthrough

    time.sleep(1)
    browser.find_element_by_xpath('//*[@id="y"]/a').click()
    time.sleep(2)
    workspace_page.do_command("top.list_connections()")
    output = workspace_page.history.split("\n")[-1]
    eq("('y', 'p1.y')" in output, True)  # verify added passthrough

    # Clean up.
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
