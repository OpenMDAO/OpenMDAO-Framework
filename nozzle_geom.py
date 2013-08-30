from openmdao.main.api import set_as_top, create
# Auto-generated file - MODIFY AT YOUR OWN RISK
a = set_as_top(create("openmdao.main.assembly.Assembly"))


# Assembly
#a.replace("driver", create("openmdao.lib.drivers.conmindriver.CONMINdriver"))
a.replace("driver", create("openmdao.lib.drivers.doedriver.DOEdriver"))

# Driver
a.driver.add("DOEgenerator", create("openmdao.lib.doegenerators.full_factorial.FullFactorial", num_levels=10))
a.driver.recorders.append(create("openmdao.lib.casehandlers.csvcase.CSVCaseRecorder"))

# GeomComponent
a.add("gc", create("openmdao.lib.components.geomcomp.GeomComponent"))
a.gc.add("parametric_geometry", create("simple_nozzle.PlugNozzleGeometry"))
a.set(u'gc.auto_run', True)

a.driver.add_parameter('gc.cowl.R[0]',low=0.0,high=1.0);
#a.driver.add_objective('gc.cowl.R[0]')
#a.driver.error_policy = 'RETRY'
a.run()


# gc.set(u'cowl.R', [2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
# gc.set(u'auto_run', True)
# gc.set(u'cowl.R', [3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
# gc.set(u'cowl.X', [2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
# gc.set(u'cowl.X', [0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
# gc.set(u'cowl.R', [0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
# a = set_as_top(create("openmdao.main.assembly.Assembly"))
# a.add("gc", create("openmdao.lib.components.geomcomp.GeomComponent"))
# a.gc.add("parametric_geometry", create("simple_nozzle.PlugNozzleGeometry"))
# a.driver.add("DOEgenerator", create("openmdao.lib.doegenerators.full_factorial.FullFactorial", num_levels=10))
# a.driver.recorders.append(create("openmdao.lib.casehandlers.csvcase.CSVCaseRecorder"))
# a.set(u'gc.auto_run', True)
# a.driver.add_parameter('gc.cowl.R[0]',low=0.0,high=1.0);
