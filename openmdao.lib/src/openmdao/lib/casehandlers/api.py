"""

.. _`openmdao.lib.casehandler.api.py`:

A central place to access all of the OpenMDAO case recorders, case
iterators, and case filters in the standard library.
"""

from openmdao.lib.casehandlers.caseset import CaseArray, CaseSet, caseiter_to_caseset

from openmdao.lib.casehandlers.csvcase import CSVCaseIterator, CSVCaseRecorder
from openmdao.lib.casehandlers.dbcase import DBCaseIterator, DBCaseRecorder, \
                                             case_db_to_dict
from openmdao.lib.casehandlers.dumpcase import DumpCaseRecorder

from openmdao.lib.casehandlers.jsoncase import JSONCaseRecorder, \
                                               BSONCaseRecorder, verify_json

from openmdao.lib.casehandlers.listcase import ListCaseRecorder, \
                                               ListCaseIterator

from openmdao.lib.casehandlers.caseset import CaseArray, CaseSet, \
                                              caseiter_to_caseset

from openmdao.lib.casehandlers.filters import SequenceCaseFilter, \
                                              SliceCaseFilter, ExprCaseFilter

from openmdao.lib.casehandlers.query import CaseDataset

from openmdao.lib.casehandlers.csv_post_processor import caseset_query_to_csv
from openmdao.lib.casehandlers.dump_post_processor import caseset_query_dump
from openmdao.lib.casehandlers.html_post_processor import caseset_query_to_html

try:
    from openmdao.lib.casehandlers.query_hdf5 import CaseDatasetHDF5
    from openmdao.lib.casehandlers.hdf5case import HDF5CaseRecorder
except ImportError:
    pass
