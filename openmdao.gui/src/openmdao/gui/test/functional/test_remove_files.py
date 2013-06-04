"""
Tests of overall workspace functions.
"""

import time

import pkg_resources

from nose import SkipTest
from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import StaleElementReferenceException, \
                                       WebDriverException
from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.basepageobject import TMO
from pageobjects.slot import find_slot_figure
from pageobjects.util import ArgsPrompt, NotifierPage
from pageobjects.workspace import WorkspacePage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser



def _test_removefiles(browser):
    # Adds multiple files to the project.
    project_dict, workspace_page = startup(browser)

    # Add some files
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')
    optPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                              'optimization_unconstrained.py')
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

    # delete using context menu the file paraboloid.py
    workspace_page.delete_file('paraboloid.py')
    
    # Check to make sure the file was deleted
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', ]
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

    # Test deleting the paraboloid and opt files at one time using the delete files pick
    #   on the Files menu
    workspace_page.delete_files( [ 'vehicle_singlesim.py', 'optimization_unconstrained.py' ] )

    # Check to make sure the files were deleted
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['basic_model.py' ]
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Test deleting a file in a folder
    workspace_page.new_folder( "test_folder" )
    workspace_page.add_file_to_folder( "test_folder", paraboloidPath )
    workspace_page.expand_folder( 'test_folder' )
    workspace_page.delete_files( [ 'test_folder/paraboloid.py', ] )

    # Check to make sure the file was deleted
    time.sleep(1.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['basic_model.py' ]
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Clean up.
    closeout(project_dict, workspace_page)



if __name__ == '__main__':
    main()
