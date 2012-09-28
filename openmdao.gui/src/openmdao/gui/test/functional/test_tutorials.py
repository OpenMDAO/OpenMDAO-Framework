"""
Functional testing of the tutorial problems in the GUI.
"""

import pkg_resources
import sys
import time
from unittest import TestCase

from nose.tools import eq_ as eq
from nose.tools import with_setup

if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     startup, closeout
    from pageobjects.util import NotifierPage
    from pageobjects.workspace import WorkspacePage
    from test_dragdrop import replace_driver, drag_element_to, release

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser
            
def _test_MDAO_MDF(browser):
    # Build the MDF model as per the tutorial.
    
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)
    
    # Import the files that contain the disciplines
    file_path = pkg_resources.resource_filename('openmdao.lib.optproblems',
                                                'sellar.py')
    workspace_page.add_file(file_path)
    
    # Add Disciplines to assembly.
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow(
        'sellar.Discipline1', 'dis1')
    workspace_page.add_library_item_to_dataflow(
        'sellar.Discipline2', 'dis2')

    # Replace Run_Once with SLSQP
    replace_driver(workspace_page, 'top', 'SLSQPdriver')
    
    # Add Solver
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.drivers.iterate.FixedPointIterator',
        'solver', offset=(50, 50))
    
    # One data connection
    dis1 = workspace_page.get_dataflow_figure('dis1', 'top')
    dis2 = workspace_page.get_dataflow_figure('dis2', 'top')
    conn_page = workspace_page.connect(dis1, dis2)
    conn_page.connect_vars('dis1.y1', 'dis2.y1')
    conn_page.close()
    
    # Add solver to optimizer workflow
    workspace_page('workflow_tab').click()
    workspace_page.show_workflow('top')
    time.sleep(0.5)
    workspace_page.expand_object('top')
    solver = workspace_page.find_object_button('top.solver')
    top = workspace_page.get_workflow_figure('top')
    chain = drag_element_to(browser, solver, top.root, False)
    release(chain)
    
    # Add disciplines to solver workflow
    workspace_page('workflow_tab').click()
    workspace_page.show_workflow('top')
    time.sleep(0.5)
    workspace_page.expand_object('top.solver')
    dis1 = workspace_page.find_object_button('top.dis1')
    solver = workspace_page.get_workflow_figure('solver')
    chain = drag_element_to(browser, dis1, solver.root, False)
    release(chain)
    workspace_page('workflow_tab').click()
    workspace_page.show_workflow('top')
    time.sleep(0.5)
    workspace_page.expand_object('top.solver')
    dis2 = workspace_page.find_object_button('top.dis2')
    solver = workspace_page.get_workflow_figure('solver')
    chain = drag_element_to(browser, dis2, solver.root, False)
    release(chain)
    
    # Configure Solver
    workspace_page('dataflow_tab').click()
    driver = workspace_page.get_dataflow_figure('solver', 'top')
    editor = driver.editor_page(base_type='Driver')
    
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'dis1.y2'
    dialog.low = '-9.e99'
    dialog.high = '9.e99'
    dialog('ok').click()
    
    editor('constraints_tab').click()
    dialog = editor.new_constraint()
    dialog.expr = 'dis2.y2 - dis1.y2 = 0'
    dialog('ok').click()    
    editor.close()
    
    # Configure Optimizer
    workspace_page('dataflow_tab').click()
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'dis1.z1,dis2.z1'
    dialog.low = '-10.0'
    dialog.high = '10.0'
    dialog('ok').click()
    
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'dis1.z2,dis2.z2'
    dialog.low = '0.0'
    dialog.high = '10.0'
    dialog('ok').click()
    
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = "dis1.x1"
    dialog.low = '0.0'
    dialog.high = '10.0'
    dialog('ok').click()
    
    editor('constraints_tab').click()
    dialog = editor.new_constraint()
    dialog.expr = '3.16 < dis1.y1'
    dialog('ok').click()    
    
    editor('constraints_tab').click()
    dialog = editor.new_constraint()
    dialog.expr = 'dis2.y2 < 24.0'
    dialog('ok').click()    
    
    editor('objectives_tab').click()
    dialog = editor.new_objective()
    dialog.expr = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)'
    dialog('ok').click()    
    editor.close()
    
    # Run the model
    workspace_page.run()
    
    # Check the objective
    workspace_page.do_command("top.dis1.z1")
    output1 = workspace_page.history.split("\n")[-1]
    workspace_page.do_command("top.dis1.z2")
    output2 = workspace_page.history.split("\n")[-1]

    if abs(float(output1) - 1.977657) > 0.01:
        raise TestCase.failureException(
            "Parameter z1 did not reach correct value, but instead is %s"
            % output1)

    if abs(float(output2) - 0.0) > 0.0001:
        raise TestCase.failureException(
            "Parameter z2 did not reach correct value, but instead is %s"
            % output2)

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)

    
if __name__ == '__main__':
    main()    