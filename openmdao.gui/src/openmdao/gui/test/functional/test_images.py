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
    file1_name = 'Engine_Example_Process_Diagram.png'
    file1_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                 'files/' + file1_name)
    workspace_page.add_file(file1_path)
    time.sleep(1)

    # view the image file
    images_page = workspace_page.view_image(file1_name)
    time.sleep(2)

    # the image will appear twice.. in the main view and as a thumbnail
    image_names = images_page.get_image_names()
    eq(len(image_names), 2)
    eq(image_names[0].endswith(file1_name), True)  # main view, "stage"
    eq(image_names[1].endswith(file1_name), True)  # thumb #1

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # add an image file to a folder
    workspace_page.new_folder('folder')
    time.sleep(1)
    file2_name = 'bmp_24.bmp'
    file2_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                 'files/' + file2_name)
    workspace_page.add_file_to_folder('folder', file2_path)
    time.sleep(1)

    # view the image file
    workspace_page.expand_folder('folder')
    time.sleep(1)
    images_page = workspace_page.view_image('folder/'+file2_name)
    time.sleep(2)

    # the image will appear twice.. in the main view and as a thumbnail
    image_names = images_page.get_image_names()
    eq(len(image_names), 3)
    eq(image_names[0].endswith(file2_name), True)  # main view, "stage"
    eq(image_names[1].endswith(file2_name), True)  # thumb #1
    eq(image_names[2].endswith(file1_name), True)  # thumb #2

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
