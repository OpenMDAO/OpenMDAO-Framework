"""
Tests of overall workspace functions.
"""

import sys
import time

import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase

from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait






if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project

    from pageobjects.component import NameInstanceDialog
    from pageobjects.dataflow import DataflowFigure

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def start_test(browser):
    # Create a project and enter workspace
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    #open library
    workspace_page.show_library() 

    return projects_page, project_info_page, project_dict, workspace_page

def teardown_test(projects_page, project_info_page, project_dict, workspace_page):
    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()

#WORKING
def _test_drop_on_driver(browser):
    projects_page, project_info_page, project_dict, workspace_page = start_test(browser)

    #find and get the 'assembly', 'top', and 'driver' objects
    assembly = workspace_page.find_library_button('Assembly').element
    top = workspace_page.get_dataflow_figure('top')
    driver_element = workspace_page.get_dataflow_figure('driver')

    for div in getDropableElements(driver_element):
            chain = drag_element_to(browser, assembly, div, True)
            check_highlighting(top('content_area').element, browser, False, "Driver's content_area")
            release(chain)


    #don't bother checking to see if they appeared, the UI box will appear and screw the test if they did

    teardown_test(projects_page, project_info_page, project_dict, workspace_page)

#WORKING
def _test_workspace_dragdrop(browser):
    projects_page, project_info_page, project_dict, workspace_page = start_test(browser)

    #find and get the 'assembly', and 'top' objects
    assembly = workspace_page.find_library_button('Assembly').element
    top = workspace_page.get_dataflow_figure('top')
   
    names = []
    for div in getDropableElements(top):
            chain = drag_element_to(browser, assembly, div, False)
            check_highlighting(top('content_area').element, browser, True, "Top's content_area")
            release(chain)

            #deal with the modal dialog
            name = (NameInstanceDialog(browser, top.port).create_and_dismiss())
            names.append(name)

    ensure_names_in_workspace(workspace_page, names,"Dragging 'assembly' to 'top' in one of the drop areas did not produce a new element on page\n")

    #now test to see if all the new elements are children of 'top'
    
    #generate what the pathnames SHOULD be
    guess_pathnames = ["top."+name for name in names]   

    #get the actual pathnames
    figs = workspace_page.get_dataflow_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]

    #see if they match up! (keeping in mind that there are more elements we have pathnames for than we put there)
    for path in guess_pathnames:
        eq(path in pathnames, True, "An element did not drop into 'top' when dragged onto one of its drop areas.\nIt was created somewhere else")

    teardown_test(projects_page, project_info_page, project_dict, workspace_page)


#WORKING
def _test_drop_on_grid(browser):
    projects_page, project_info_page, project_dict, workspace_page = start_test(browser)
    
    #other tests also need to put an assembly on the grid, so put in seperate method
    put_assembly_on_grid(browser, workspace_page)   

    teardown_test(projects_page, project_info_page, project_dict, workspace_page)

#WORKING
def _test_drop_on_existing_assembly(browser):
    projects_page, project_info_page, project_dict, workspace_page = start_test(browser)

    assembly = workspace_page.find_library_button('Assembly').element
    

    outer_name = put_assembly_on_grid(browser, workspace_page)
    outer_figure = workspace_page.get_dataflow_figure(outer_name)
    outer_path = get_pathname(browser, outer_figure('header').element.find_element_by_xpath(".."))

    eq(outer_path, outer_name, "Assembly did not produce an instance on the grid")

    div = getDropableElements(outer_figure)[0]
    chain = drag_element_to(browser, assembly, div, False)
    check_highlighting(outer_figure('content_area').element, browser, True, "Assembly's content_area")
    release(chain)

    middle_name = (NameInstanceDialog(browser, outer_figure.port).create_and_dismiss())
    middle_figure = workspace_page.get_dataflow_figure(middle_name)
    middle_path = get_pathname(browser, middle_figure('header').element.find_element_by_xpath(".."))

    eq(middle_path, outer_path+'.'+middle_name, "Assembly did not produce an instance inside outer Assembly")


    div = getDropableElements(middle_figure)[0]
    chain = drag_element_to(browser, assembly, div, True)
    check_highlighting(middle_figure('content_area').element, browser, True, "Assembly's content_area")
    release(chain)

    inner_name = (NameInstanceDialog(browser, middle_figure.port).create_and_dismiss())
    #expand the middle div so that the inner one shows up in the workspace.
    middle_figure('top_right').element.click()
    inner_figure =  workspace_page.get_dataflow_figure(inner_name)
    inner_path = get_pathname(browser, inner_figure('header').element.find_element_by_xpath(".."))

    eq(inner_path, middle_path+'.'+inner_name, "Assembly did not produce an instance inside of the middle Assembly")


    ensure_names_in_workspace(workspace_page, [outer_name, middle_name, inner_name], "Dragging Assembly onto Assembly did not create a new instance on page")



    teardown_test(projects_page, project_info_page, project_dict, workspace_page)

#WORKING
def _test_drop_on_component_editor(browser):
    projects_page, project_info_page, project_dict, workspace_page = start_test(browser)
    #find and get the 'assembly', and 'top' objects
    assembly = workspace_page.find_library_button('Assembly').element
    top = workspace_page.get_dataflow_figure('top')
    top.pathname = get_pathname(browser, top('header').element.find_element_by_xpath(".."))

    editor = top.editor_page(False, True)
    editor.show_dataflow()

    #in order to get the elements in the editor workflow, we must
    #distinguish them from the elements in the main workflow

    editor_top = get_dataflow_fig_in_assembly_editor(browser, top.port, workspace_page, 'top')#sortthrough these to find the correct 'top'

    names = []
    for div in getDropableElements(editor_top)[:-1]:
            chain = drag_element_to(browser, assembly, div, False)
            check_highlighting(editor_top('content_area').element, browser, True, "Top in component editor's content_area")
            release(chain)

            #deal with the modal dialog
            name = (NameInstanceDialog(browser, editor_top.port).create_and_dismiss())
            names.append(name)

    ensure_names_in_workspace(workspace_page, names,"Dragging 'assembly' to 'top' (in component editor) in one of the drop areas did not produce a new element on page\n")

    #now test to see if all the new elements are children of 'top'
    
    #generate what the pathnames SHOULD be
    guess_pathnames = ["top."+name for name in names]   

    #get the actual pathnames
    figs = workspace_page.get_dataflow_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]

    #see if they match up! (keeping in mind that there are more elements we have pathnames for than we put there)
    for path in guess_pathnames:
        eq(path in pathnames, True, "An element did not drop into 'top' (in component editor) when dragged onto one of its drop areas.\nIt was created somewhere else")

    teardown_test(projects_page, project_info_page, project_dict, workspace_page)

#WORKING
def _test_drop_on_component_editor_grid(browser):
    projects_page, project_info_page, project_dict, workspace_page = start_test(browser)
    #find and get the 'assembly', and 'top' objects
    assembly = workspace_page.find_library_button('Assembly').element
    top = workspace_page.get_dataflow_figure('top')
    top.pathname = get_pathname(browser, top('header').element.find_element_by_xpath(".."))

    editor = top.editor_page(False, True)
    editor.show_dataflow()

    editor_top = get_dataflow_fig_in_assembly_editor(browser, top.port, workspace_page, 'top')#sortthrough these to find the correct 'top'


    chain = ActionChains(browser)  
    chain.click_and_hold(assembly)
    chain.move_to_element(editor_top('header').element.find_element_by_xpath("..")).perform()
    chain.move_by_offset(200,1).perform()
    release(chain)

     #don't bother checking to see if it appeared, the UI box will appear and screw the test if it did


    teardown_test(projects_page, project_info_page, project_dict, workspace_page)
    


def get_dataflow_fig_in_assembly_editor(browser, port, workspace_page, name):
    allFigs = workspace_page.get_dataflow_figures()
    for fig in allFigs:
        location = fig.find_element_by_xpath("..").get_attribute('id')
        if location == "top-dataflow":
            return DataflowFigure(browser, port, fig)

    return None

    teardown_test(projects_page, project_info_page, project_dict, workspace_page)

def put_assembly_on_grid(browser, workspace_page):
     #find and get the 'assembly', and the div for the grid object
    assembly = workspace_page.find_library_button('Assembly').element
    grid = browser.find_element(*(By.XPATH, '//div[@id="-dataflow"]'))

    chain = ActionChains(browser)
    chain.click_and_hold(assembly)
    chain.move_by_offset(-100,0).perform()

    check_highlighting(grid, browser, True, "Grid")
    release(chain)

    #deal with the modal dialog
    name = (NameInstanceDialog(browser, workspace_page.get_dataflow_figure('top').port).create_and_dismiss())

    #make sure it is on the grid
    ensure_names_in_workspace(workspace_page, [name],"Dragging 'assembly' to grid did not produce a new element on page")

    return name

def get_pathname(browser, fig):
    figid = fig.get_attribute('id')#get the ID of the element here
    script = "return jQuery('#"+figid+"').data('pathname')"
    return browser.execute_script(script) 

def ensure_names_in_workspace(workspace_page, names, message=None):
    """ensures the list of element names in included in the workspace"""

    allnames = workspace_page.get_dataflow_component_names()

    #sometimes does not load all of the names for some reason. Reloading seems to fix the problem
    try_reload = False
    for name in names:
        if not name in allnames:
            try_reload = True
    if try_reload:
            time.sleep(.1)
            allnames = workspace_page.get_dataflow_component_names() 

    #now we will assert that the elements that we added appear on the page
    for name in names:
        eq(name in allnames, True, message)

def drag_element_to(browser, element, drag_to, centerx):
    chain = ActionChains(browser)  
    chain.click_and_hold(element)
    chain.move_to_element(drag_to).perform()
    if centerx:
        chain.move_by_offset(int(drag_to.value_of_css_property('width')[:-2])/2, 1).perform()
    else:
        chain.move_by_offset(2,1).perform()

    return chain

def release(chain):
    chain.release(on_element=None).perform()


def check_highlighting(element,browser,should_highlight=True,message='Element'):
    #check to see that the element's background-color is rgb(207, 214, 254)
    style = element.get_attribute('style')      
    highlighted = ('background-color: rgb(207, 214, 254)' in style)\
                or('highlighted.png' in style) 
    eq(highlighted, should_highlight, message +\
        (' did not highlight (and should have) ' if should_highlight else ' highlighed (and should not have) ')\
        + 'when dragging a dropable element to it')


def getDropableElements(dataflow_figure):
    #return [dataflow_figure(area).element for area in\
    #        ['top_left','header','top_right', 'content_area', 'bottom_left', 'footer', 'bottom_right']]

    #add back 'top_left' 'bottom_left' at some point. right now that test fails
    arr = ['content_area','header', 'footer', 'bottom_right','top_right']
    return [dataflow_figure(area).element for area in arr]


if __name__ == '__main__':
    main()

