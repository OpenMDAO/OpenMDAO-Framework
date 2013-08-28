"""
Tests of overall workspace functions.
"""

import time

import pkg_resources

# these imports are for comparing screen capture to existing file
# import os
# import filecmp
# import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.gui.test.functional.util import main, \
                setup_server, teardown_server, generate, \
                startup, closeout


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_view_image(browser):
    project_dict, workspace_page = startup(browser)
    workspace_window = browser.current_window_handle

    # add an image file
    file_name = 'Engine_Example_Process_Diagram.png'
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/' + file_name)
    workspace_page.add_file(file_path)

    time.sleep(2)

    # view the image file
    images_page = workspace_page.view_image(file_name)

    time.sleep(2)

    # the image will appear twice.. in the main view and as a thumbnail
    image_names = images_page.get_image_names()
    eq(len(image_names), 2)
    eq(image_names[0].endswith(file_name), True)
    eq(image_names[1].endswith(file_name), True)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
