"""
Tests of overall workspace functions.
"""

import time

import pkg_resources

# these imports are for comparing screen capture to existing file
# import os
# import filecmp
# import pkg_resources

from unittest import TestCase

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.gui.test.functional.util import main, \
                setup_server, teardown_server, generate, \
                startup, closeout


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_addfiles(browser):
    # Adds multiple files to the project.
    project_dict, workspace_page = startup(browser)

    # Get path to  paraboloid file.
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')

    # Get path to optimization_unconstrained file.
    optPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                              'optimization_unconstrained.py')

    # Add the files
    # would like to test adding multiple files but Selenium doesn't support it
    #workspace_page.add_files(paraboloidPath, optPath)
    workspace_page.add_file(paraboloidPath)
    workspace_page.add_file(optPath)

    # Check to make sure the files were added.
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_hidden_files(browser):
    project_dict, workspace_page = startup(browser)

    # there are no visible files in a new project
    file_names = workspace_page.get_files()
    eq(len(file_names), 0)

    # show hidden files using file tree pane context menu
    workspace_page.toggle_files()

    # confirm that formerly hidden files are now visible
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    assert '_settings.cfg' in file_names

    # hide files again using file tree pane context menu
    workspace_page.toggle_files()

    # there should be no visible files
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    eq(len(file_names), 0)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_rename_file(browser):
    # Rename a file in the project.
    project_dict, workspace_page = startup(browser)

    # Add paraboloid.py
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')
    workspace_page.add_file(paraboloidPath)
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    eq(file_names, ['paraboloid.py'])

    workspace_page.rename_file('paraboloid.py', 'xyzzy.py')
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    eq(file_names, ['xyzzy.py'])

    # Clean up.
    #closeout(projects_page, project_info_page, project_dict, workspace_page)
    closeout(project_dict, workspace_page)


def _test_remove_files(browser):
    # Adds multiple files to the project.
    project_dict, workspace_page = startup(browser)

    # Add some files
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')
    optPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                              'optimization_unconstrained.py')
    workspace_page.add_file(paraboloidPath)
    workspace_page.add_file(optPath)

    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']

    # Check to make sure the files were added.
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # delete using context menu the file paraboloid.py, but cancel the confirmation
    workspace_page.delete_file('paraboloid.py', False)

    # Check to make sure the file was NOT deleted
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # delete using context menu the file paraboloid.py
    workspace_page.delete_file('paraboloid.py')

    expected_file_names = ['optimization_unconstrained.py', ]

    # Check to make sure the file was deleted
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # add more files
    file_path_one = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                    'files/basic_model.py')
    file_path_two = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                    'vehicle_singlesim.py')
    workspace_page.add_file(file_path_one)
    workspace_page.add_file(file_path_two)

    expected_file_names = ['optimization_unconstrained.py', 'basic_model.py', 'vehicle_singlesim.py']

    # Test deleting the paraboloid and opt files at one time using the delete files pick
    #   on the Files menu, but cancel the confirmation
    workspace_page.delete_files(['vehicle_singlesim.py', 'optimization_unconstrained.py'], False)

    # toggle hidden files on and off to reset selected/highlighted files
    workspace_page.toggle_files()
    workspace_page.toggle_files()

    # Check to make sure the files were NOT deleted
    time.sleep(1.5)
    file_names = workspace_page.get_files()
    print 'file names:', file_names
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Test deleting the paraboloid and opt files at one time using the delete files pick
    #   on the Files menu
    workspace_page.delete_files(['vehicle_singlesim.py', 'optimization_unconstrained.py'])

    expected_file_names = ['basic_model.py']

    # Check to make sure the files were deleted
    time.sleep(1.5)
    file_names = workspace_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Test deleting a file in a folder
    workspace_page.new_folder("test_folder")
    time.sleep(1.0)
    workspace_page.add_file_to_folder("test_folder", paraboloidPath)
    time.sleep(1.0)
    workspace_page.expand_folder('test_folder')
    time.sleep(1.0)
    workspace_page.delete_files(['test_folder/paraboloid.py', ])

    # Check to make sure the file was deleted
    time.sleep(1.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['basic_model.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_view_file(browser):
    project_dict, workspace_page = startup(browser)
    workspace_window = browser.current_window_handle

    # add an image file
    file_name = 'Engine_Example_Process_Diagram.png'
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/' + file_name)
    workspace_page.add_file(file_path)

    time.sleep(2)

    # view the image file in browser
    new_page = workspace_page.view_file(file_name)

    time.sleep(2)

    # the new page should have an img tag with the selected file name
    images = new_page.browser.find_elements_by_css_selector('img')
    eq(len(images), 1)
    eq(images[0].get_attribute('src').strip().endswith(file_name), True)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # add a pdf file
    file_name = 'sample.pdf'
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/' + file_name)
    workspace_page.add_file(file_path)

    time.sleep(2)

    # view the pdf file in browser
    new_page = workspace_page.view_file(file_name)

    time.sleep(2)

    # the new page should have an embed tag with the selected file name
    embeds = new_page.browser.find_elements_by_css_selector('embed')
    eq(len(embeds), 1)
    eq(embeds[0].get_attribute('src').strip().endswith(file_name), True)
    eq(embeds[0].get_attribute('type'), 'application/pdf')

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


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

    # check that the image is displayed
    image = images_page.get_image()
    eq(image.endswith(file1_name), True)

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

    # check that the image is displayed
    image = images_page.get_image()
    eq(image.endswith(file2_name), True)

    # check that both images appear in the thumbnails
    thumbnails = images_page.get_thumbnails()
    thumbnails.sort()
    eq(len(thumbnails), 2)
    eq(thumbnails[0].endswith(file1_name), True)
    eq(thumbnails[1].endswith(file2_name), True)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
