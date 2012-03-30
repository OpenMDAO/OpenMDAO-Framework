
import sys
import optparse
import pprint

import cPickle as pickle

import zmq
from zmq.eventloop import ioloop, zmqstream

def handle_msg(msg):
    try:
        print 'received: %s' % [msg[0], pickle.loads(msg[1])]
    except Exception as err:
        print str(err)

def main(args):
    parser = optparse.OptionParser()
    parser.add_option("-u", "--url", action="store", type="string", dest='url', 
                      help="url of pub socket", default='tcp://*:5556')
    parser.add_option("-t", "--topic", action="append", type="string", dest='topics', 
                      help="topic to listen for", default=[])

    (options, args) = parser.parse_args(args)
    
    url = options.url
    
    # Prepare our context and sockets
    context = zmq.Context()
    
    # Socket to talk to pub socket
    subsock = context.socket(zmq.SUB)
    
    if url.startswith('ws:'):
        from openmdao.main.zmqws import PubWebSocketHandler
        application = web.Application([(wsroute, CompWebSocketHandler, {'context':context,
                                                                        'rep_url':rep_url,
                                                                        'pub_url':pub_url})])
        application.listen(port)
    else:  # regular zmq socket
        subsock.connect(url)
        
    if options.topics:
        for t in options.topics:
            subsock.setsockopt(zmq.SUBSCRIBE, t)
    else:
        subsock.setsockopt(zmq.SUBSCRIBE, '')

    stream = zmqstream.ZMQStream(subsock)
    stream.on_recv(handle_msg)
        
    loop = ioloop.IOLoop.instance()
    loop.start()

if __name__ == "__main__":
    main(sys.argv[1:])
    