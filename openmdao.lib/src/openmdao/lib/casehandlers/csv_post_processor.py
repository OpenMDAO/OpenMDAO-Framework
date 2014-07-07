"""
Post-processing function that takes a case_data_set and outputs a csv file

 

Should be able to pass tests of current csv case recorder (column ordering, meta column, etc...)

Assume query by case (not variable)


"""

import cStringIO
import StringIO
import logging
import sys
import time

import json
import bson

from numpy  import ndarray
from struct import pack
from uuid   import uuid1

from openmdao.lib.casehandlers.api import CaseDataset, \
                                          JSONCaseRecorder, BSONCaseRecorder

def caseset_query_to_csv(query):
	"""
	Create a csv file from the results of a case_data_set 
	query so the data can be imported to other tools

	query is a Query object
	"""

	f = query.fetch()
	#print f

