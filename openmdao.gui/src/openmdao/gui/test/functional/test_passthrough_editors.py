"""
Tests of passthrough editor functions.
"""

import pkg_resources
import sys
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

    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.add_library_item_to_dataflow('passthrough_editors.Topp', "top")

    time.sleep(2)
    top = workspace_page.get_dataflow_figure("top")
    top._context_click('edit_passthroughs')

    expand_i = '//*[@id="PassthroughsFrame-top-passthrough-input-table-div"]/ul/li[3]/ins'
    expand_o = '//*[@id="PassthroughsFrame-top-passthrough-output-table-div"]/ul/li[3]/ins' 
    
    browser.find_element_by_xpath(expand_i).click()
    time.sleep(1)
    y_box = '//*[@id="top-p1-yinput-cbchb"]'
    y_btn = browser.find_element_by_xpath(y_box)
    eq(y_btn.is_selected(), True)  # check existing passthrough
    
    browser.find_element_by_xpath(expand_o).click()
    time.sleep(1)
    f_xy_box = '//*[@id="top-p1-f_xyoutput-cbchb"]'
    f_xy_btn = browser.find_element_by_xpath(f_xy_box)
    eq(f_xy_btn.is_selected(), False)  # verify passthrough doesn't exist yet
    
    time.sleep(1)
    browser.find_element_by_xpath('//*[@id="top-p1-yinput' \
                                  '-cb"]/ins[1]').click()  # remove passthrough
    time.sleep(1)
    browser.find_element_by_xpath('//*[@id="top-p1-f_xyoutput' \
                                  '-cb"]/ins[1]').click()  # create passthrough
    time.sleep(3)

    workspace_page.do_command("top.list_connections()")
    time.sleep(.5)
    output = workspace_page.history.split("\n")[-1]
    eq("('p1.f_xy', 'f_xy')" in output, True)  # verify created passthrough
    eq("('y', 'p1.y')" in output, False)  # verify removed passthrough

    # Clean up.
    closeout(project_dict, workspace_page)
    

if __name__ == '__main__':
    main()
