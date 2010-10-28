"""Psudo package providing a central place to access all of the
OpenMDAO caserecorders standard library"""

# CaseRecorders
from openmdao.lib.caserecorders.dbcaserecorder import DBCaseRecorder
from openmdao.lib.caserecorders.dumpcaserecorder import DumpCaseRecorder

from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict