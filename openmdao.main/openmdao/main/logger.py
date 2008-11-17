"""
This is just a wrapper for the logging module so we can switch it out later
if needed.
"""

#public symbols
#__all__ = ['logger']

__version__ = '0.1'
#__author__ = ""


import logging


# set up logging to file 
logging.basicConfig(level=logging.DEBUG,
#                    format='%(asctime)s %(name)-8s %(levelname)-5s %(message)s',
                    format='%(asctime)s %(name)s %(levelname)s %(message)s',
#                    datefmt='%m-%d %H:%M',
                    datefmt='%H:%M',
                    filename='openmdao_log.txt',
                    filemode='w')
                    

logger = logging.getLogger('')


# define a Handler which writes INFO messages or higher to the sys.stderr
console = logging.StreamHandler()
console.setLevel(logging.DEBUG)
# set a format which is simpler for console use
#formatter = logging.Formatter('%(name)-12s: %(levelname)-8s %(message)s')
formatter = logging.Formatter('%(levelname)-6s %(message)s')
# tell the handler to use this format
console.setFormatter(formatter)
# add the handler to the root logger
#logger.addHandler(console)
