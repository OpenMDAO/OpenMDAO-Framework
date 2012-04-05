import string
import random
import time

from selenium.webdriver.common.keys import Keys
from selenium.webdriver import ActionChains
from selenium.webdriver.support.ui import WebDriverWait # available since 2.4.0
from selenium.common.exceptions import NoSuchElementException
#from pageobjects import locators
#from pageobjects.basepageobject import BasePageObject
from basepageobject import BasePageObject
#from pageobjects.basepageelement import BasePageElement

from selenium.webdriver.common.by import By


# do NOT put asserts in this file! Those go in the tests

class ProjectObject(BasePageObject):
    '''A super class for the new project page
    and the project info page where details of the
    project can be edited
    '''
    
    def __init__(self, browser, port):

        super(ProjectObject, self).__init__(browser,port)
        self.locators = {}
        self.locators["project_name"] = ( By.ID, "id_projectname" )
        self.locators["description"] = ( By.ID, "id_description" )
        self.locators["version"] = ( By.ID, "id_version" )
        self.locators["shared"] = ( By.ID, "id_shared" )

    @property
    def project_name(self):
        return self._project_name
    @project_name.setter
    def project_name(self, project_name):
        self.browser.find_element(*self.locators["project_name"]).clear()
        self.browser.find_element(*self.locators["project_name"]).send_keys(project_name)
        self._project_name = project_name

    @property
    def description(self):
        return self._description
    @description.setter
    def description(self, description):
        self.browser.find_element(*self.locators["description"]).clear()
        self.browser.find_element(*self.locators["description"]).send_keys(description)
        self._description = description

    @property
    def version(self):
        return self._version
    @version.setter
    def version(self, version):
        self.browser.find_element(*self.locators["version"]).clear()
        self.browser.find_element(*self.locators["version"]).send_keys(version)
        self._version = version

    @property
    def shared(self):
        return self._shared
    @shared.setter
    def shared(self, shared):
        shared_checkbox = self.browser.find_element(*self.locators["shared"])
        if shared != shared_checkbox.is_selected() :
            shared_checkbox.click() # toggle it
        self._shared = shared


class NewProjectObject(ProjectObject):

    def __init__(self, browser, port):

        super(NewProjectObject, self).__init__(browser,port)
        self.locators["create_project"] = ( By.XPATH, "/html/body/div/div[2]/form/input" )
        self._page_title = "New Project"

    def submit(self):
        self.browser.find_element(*self.locators["create_project"]).click()

    def get_random_project_name(self,size=6, chars=string.ascii_uppercase + string.digits):
        return "testing project " + ''.join(random.choice(chars) for x in range(size))

    def create_project( self, project_name, description, version, shared ) :
        self.project_name = project_name
        self.description = description
        self.version = version
        self.shared = shared
        self.submit()
        return ProjectInfoObject( self.browser, self.port )

class ProjectInfoObject(ProjectObject):

    def __init__(self, browser, port):

        super(ProjectInfoObject, self).__init__(browser,port)
        self.locators["project_title"] = ( By.XPATH, "/html/body/div/div[2]/h1" )
        self.locators["update_project_details"] = ( By.XPATH, "/html/body/div/div[2]/form/input" )
        self.locators["load_project_into_workspace"] = ( By.XPATH, "/html/body/div/div[2]/form[3]/input[2]" )
        self.locators["logout"] = ( By.LINK_TEXT, "Logout" )
        self.locators["back_to_projects"] = ( By.LINK_TEXT, "Back to Projects" )

        self.assert_on_correct_page()

    def go_to_projects_page( self ):
        self.browser.find_element(*self.locators["back_to_projects"]).click()
        return ProjectsListPageObject( self.browser, self.port )

    def load_project_into_workspace( self ):
        self.browser.find_element(*self.locators["load_project_into_workspace"]).click()
        return WorkspacePageObject( self.browser, self.port )

    def logout(self):
        self.browser.find_element(*self.locators["logout"]).click()
        return LoginPageObject( self.browser, self.port )


class WorkspacePageObject(BasePageObject):

    def __init__(self, browser, port):

        super(WorkspacePageObject, self).__init__(browser,port)

        self.locators = {}
        #self.locators["close"] = ( By.XPATH, "/html/body/nav/ul/li/ul/li[4]/a" )
        self.locators["project_menu"] = ( By.XPATH, "/html/body/nav/ul/li/a" )
        self.locators["files_tab"] = ( By.XPATH, '//*[@id="ftree_tab"]' )
        self.locators["file_menu"] = ( By.XPATH, '/html/body/div/dl/dd[2]/div/nav2/ul/li/a' )
        self.locators["add_file_menu_item"] = ( By.XPATH, '/html/body/div/dl/dd[2]/div/nav2/ul/li/ul/li[3]/a' )
        self.locators["add_file_input"] = ( By.NAME, 'myfile' )
        self.locators["add_file_submit"] = ( By.XPATH, '/html/body/div/div[2]/form/input[2]' )
        self.locators["import_file"] = ( By.XPATH, "//a[(@rel='importfile')]" )
        self.locators["palette_tab"] = ( By.ID, "palette_tab" )
        self.locators["working_section"] = ( By.XPATH, "//div[(@id='palette')]//div[(@title='working')]" )
        self.locators["dataflow_component_headers"] = ( By.XPATH, "//div[@class='DataflowComponentFigureHeader']" )
        self.locators["files"] = ( By.XPATH, "//div[@id='ftree']//a[@class='file ui-draggable']" )
        self._page_title = "OpenMDAO: " # followed by user name

    def close_workspace(self):
        self.browser.execute_script("document.getElementById('mainmenu-close').click();")
        
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element(*self.locators["project_menu"]) ).click() # Project menu
        # action_chains = ActionChains(self.browser)
        # close = self.browser.find_element(*self.locators["close"])
        # action_chains.move_to_element( close ).click_and_hold( close ).release( close ).perform()
        return ProjectsListPageObject(self.browser, self.port)

    def save_project(self):
        self.browser.execute_script("document.getElementById('mainmenu-save').click();")

    def view_workflow( self ):
        '''Since the workflow menu is hidden, need to use JavaScript since
        Selenium 2 cannot see hidden items'''
        
        self.browser.execute_script("document.getElementById('mainmenu-workflow').click();")

    def view_code_editor( self ):
        '''Since the workflow menu is hidden, need to use JavaScript since
        Selenium 2 cannot see hidden items'''
        
        self.browser.execute_script("document.getElementById('mainmenu-code-editor').click();")

    def view_structure( self ):
        self.browser.execute_script("document.getElementById('mainmenu-structure').click();")

    def view_refresh( self ):
        self.browser.execute_script("document.getElementById('mainmenu-refresh').click();")

        # move the View menu out of the way in case it is in the way
        self.browser.execute_script("jQuery( document.getElementById('mainmenu-view').nextSibling ).slideUp();")

    def files_tab( self ):
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element(*self.locators["files_tab"]) ).click()

    def get_files( self ):
        '''Warning: the workspace needs to be refreshed to make sure
           this command can see everything'''
        file_items = self.browser.find_elements(*self.locators["files"])
        file_names = []
        for f in file_items:
            file_names.append( f.text[1:] )
        return file_names
    
    def add_file( self, file_path ):
        ''' read in a file '''

        self.files_tab()
        
        # Click File ^ menu - need to do this to make the menu item visible
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element(*self.locators["file_menu"]) ).click()

        # Click Add File menu item
        time.sleep(3)
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element(*self.locators["add_file_menu_item"]) ).click()

        # Switch to the Window that pops up
        main_window_handle = self.browser.current_window_handle
        self.browser.switch_to_window( 'Add File' )

        file_input_element = WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element(*self.locators["add_file_input"]) )

        file_input_element.send_keys(file_path)

        # Submit
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element(*self.locators["add_file_submit"]) ).click()

        # go back to the main window
        self.browser.switch_to_window( main_window_handle )

    def import_from_file(self, filename) :
        f = self.browser.find_element_by_xpath("//a[(@path='/%s')]" % filename )
        
        action_chains = ActionChains(self.browser)
        action_chains.context_click( f ).perform()
        
        #WebDriverWait(self.browser,10).until(
        #    lambda browser:self.browser.find_element(*self.locators["import_file"]) ).click()
        self.browser.find_element(*self.locators["import_file"]).click()

    def libraries_tab( self ):
        self.browser.find_element(*self.locators["palette_tab"]).click()

    def working_section( self ):
        time.sleep(2)
        self.browser.find_element(*self.locators["working_section"]).click()

    def add_library_item_to_structure( self, item_name, user_name_for_item ):
        library_item = self.browser.find_element_by_xpath(
            "//div[(@id='palette')]//div[(@path='%s')]" % item_name )
        structure_pane_target = self.browser.find_element_by_xpath("//*[@id='-dataflow']")

        #self.browser.execute_script("$('div.-dataflow').height(5000);")
        #self.browser.execute_script("document.getElementById('-dataflow');")
        #self.browser.execute_script("document.getElementById('-dataflow').style.height = 5000;")
        #self.browser.execute_script("document.getElementById('-dataflow').style.width = 5000;")

        time.sleep(2)

        #action_chains = ActionChains(self.browser)
        #action_chains.drag_and_drop(paraboloid_component, structure_pane_target).perform();
        
        action_chains = ActionChains(self.browser)
        action_chains.click_and_hold(library_item).move_to_element_with_offset(structure_pane_target,100,100).release(None).perform();

        # popup div is displayed
        name_input_element = WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/div[12]/div[2]/input') )
        name_input_element.send_keys(user_name_for_item)

        # Then say OK
        name_input_element.send_keys(Keys.RETURN)

    def get_number_of_dataflow_components(self):
        dataflow_components = self.browser.find_elements_by_class_name('DataflowComponentFigure')
        return len( dataflow_components )

    def get_dataflow_component_names(self):
        dataflow_component_headers = \
            self.browser.find_elements(
            *self.locators["dataflow_component_headers"])
        names = []
        for header in dataflow_component_headers :
            names.append( header.text )
        return names
    
class ProjectsListPageObject(BasePageObject):

    def __init__(self, browser, port):

        super(ProjectsListPageObject, self).__init__(browser,port)
        self.locators = {}
        self.locators["logout"] = ( By.LINK_TEXT, "Logout" )
        self.locators["new project"] = ( By.CSS_SELECTOR, "html body div#body div.content p a" )
        
        self._page_title = "Projects"

    def logout(self):
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element(*self.locators["logout"]) ).click()
        #self.browser.find_element(*self.locators["logout"]).click()
        return LoginPageObject( self.browser, self.port )

    def new_project(self):
        self.browser.find_element(*self.locators["new project"]).click()
        return NewProjectObject( self.browser, self.port )

    def is_project_with_this_name_on_list(self,project_name):
        num_projects_with_given_name = self.browser.find_elements_by_link_text( project_name )
        return num_projects_with_given_name > 0

    def open_project(self, project_name) :
        self.browser.find_element_by_link_text( project_name ).click()
        return ProjectInfoObject( self.browser, self.port )
        
    

class LoginPageObject(BasePageObject):

    def __init__(self, browser, port):

        super(LoginPageObject, self).__init__(browser,port)
        #self.browser.get("http://localhost:%d/accounts/login/?next=/" % self.port)

        # TODO: Move the locators from global space to the page objects themselves
        self.locators = {}
        self.locators["username"] = ( By.ID, "id_username" )
        self.locators["password"] = ( By.ID, "id_password" )
        self.locators["submit"] = ( By.XPATH, "/html/body/div/div[2]/form/input" )

        self._page_title = "Login"
        self._page_url = "http://localhost:%d/accounts/login/?next=/" % self.port


    # username prop #
    @property
    def username(self):
        return self._username

    @username.setter
    def username(self, username):
        self.browser.find_element(*self.locators["username"]).clear()
        self.browser.find_element(*self.locators["username"]).send_keys(username)
        self._username = username

    # password prop #
    @property
    def password(self):
        return self._password

    @password.setter
    def password(self, password):
        self.browser.find_element(*self.locators["password"]).clear()
        self.browser.find_element(*self.locators["password"]).send_keys(password)
        self._password = password

    def login_successfully( self, username, password ) :
        self.username = username
        self.password = password
        self.submit()
        return ProjectsListPageObject( self.browser, self.port )

    def login_unsuccessfully( self, username, password ) :
        self.username = username
        self.password = password
        self.submit()
        return LoginPageObject( self.browser, self.port )

    def magic_login(self, username, password ):
        '''Need a way to login to the app directly,
        not manually via the GUI'''
        pass
    
    @property
    def page_title(self):
        WebDriverWait(self.browser, 10).until(lambda s: self.browser.title)
        return self.browser.title

    def submit(self):
        #wait_for = "selenium.browserbot.getCurrentWindow().document.getElementById('LogoutButton')"
        #locator = locators["openmdao_login.submit"]
        #self.browser.find_element_by_xpath(locators["openmdao_login.submit"]).click()
        self.browser.find_element(*self.locators["submit"]).click()
        #self.se.wait_for_condition(wait_for, "30000")


