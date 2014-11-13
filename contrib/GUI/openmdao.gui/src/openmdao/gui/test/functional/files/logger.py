import logging

for i in range (1, 4):
    logging.debug('debug %d', i)
    logging.info('info %d', i)
    logging.warning('warning %d', i)
    logging.error('error %d', i)
    logging.critical('critical %d', i)
