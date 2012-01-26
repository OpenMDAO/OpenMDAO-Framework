
import sys
import optparse
import pprint

import zmq

def main(args):
    parser = optparse.OptionParser()
    parser.add_option("-u", "--url", action="store", type="string", dest='url', 
                      help="url of command socket", default='tcp://127.0.0.1:5555')

    (options, args) = parser.parse_args(args)
    
    url_cmd = options.url
    
    # Prepare our context and sockets
    context = zmq.Context()
    
    # Socket to talk to command sockets
    cmdsock = context.socket(zmq.REQ)
    cmdsock.connect(url_cmd)
    
    kwargs = {}
    for i,arg in enumerate(args):
        if '=' in arg:
            parts = arg.split('=')
            kwargs[parts[0]] = parts[1]
        elif ',' in arg:
            args[i] = arg.split(',')
            
    fname = args[0]
    fargs = [] if len(args)<2 else args[1:len(args)-len(kwargs)]
    
    msg = [fname, fargs, kwargs]
    
    print "sending: ",
    pprint.pprint(msg)
    
    cmdsock.send_pyobj(msg)
    
    received = cmdsock.recv_pyobj()
    
    print "received: %s" % received
    
    cmdsock.close()

if __name__ == "__main__":
    main(sys.argv[1:])
    