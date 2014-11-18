import os
import sys
import subprocess
import traceback

from optparse import OptionParser

import zmq
from zmq.eventloop import ioloop

from openmdao.main.zmqcomp import ZmqCompWrapper
from openmdao.gui.outstream import OutStream

debug = True


def DEBUG(msg):
    if debug:
        print '<<<' + str(os.getpid()) + '>>> ZMQServer --', msg
        sys.stdout.flush()


class ZMQServer(object):
    ''' Wraps an an openmdao object with ZMQ and runs it as a server.
    '''

    def __init__(self, options):
        self.options = options

        parts = self.options.classpath.split('.')
        modpath = '.'.join(parts[:-1])
        __import__(modpath)
        try:
            mod = sys.modules[modpath]
            self.ctor = getattr(mod, parts[-1])
        except KeyError:
            print "ZMQServer can't locate %s" % self.options.classpath

    def serve(self):
        DEBUG(self.options.classpath)
        # redirect stdout/stderr to a ZMQ socket
        self.sysout = sys.stdout
        self.syserr = sys.stderr
        try:
            context = zmq.Context()
            socket = context.socket(zmq.PUB)
            DEBUG('binding output to ' + self.options.out_url)
            socket.bind(self.options.out_url)
            sys.stdout = OutStream(socket, 'stdout')
            sys.stderr = sys.stdout
        except Exception:
            print >> self.sysout, \
                  '<<<%s>>> ZMQServer -- setup on %s failed:' \
                  % (os.getpid(), self.options.out_url)
            traceback.print_exc(file=self.sysout)
            sys.exit(1)

        try:
            self.obj = self.ctor()
            DEBUG('obj=' + str(self.obj))
            ZmqCompWrapper.serve(self.obj,
                                 rep_url=self.options.rep_url,
                                 pub_url=self.options.pub_url)
        except Exception:
            print >> self.sysout, \
                  '<<<%s>>> ZMQServer -- wrapper failed:' % os.getpid()
            traceback.print_exc(file=self.sysout)
            sys.exit(1)

    @staticmethod
    def get_options_parser():
        ''' Create a parser for command line arguments.
        '''
        parser = OptionParser()
        parser.add_option("-c", "--class",
                          dest='classpath',
                          help="module path to class of top level component")
        parser.add_option("-r", "--rep_url",
                          dest="rep_url",
                          help="the address of the RPC proxy")
        parser.add_option("-p", "--pub_url",
                          dest="pub_url",
                          help="the address or the publisher")
        parser.add_option("-o", "--out_url",
                          dest="out_url",
                          help="the address of the output stream")
        return parser

    @staticmethod
    def spawn_server(classpath, rep_url, pub_url, out_url):
        ''' Run server in its own process.
        '''
        file_path = os.path.abspath(__file__)
        cmd = ['python', file_path,
               '-c', str(classpath),
               '-r', str(rep_url),
               '-p', str(pub_url),
               '-o', str(out_url)]

        return subprocess.Popen(cmd)


def main():
    ''' Process command line arguments, create server, and start it up.
    '''
    # make sure to install zmq ioloop before creating any tornado objects
    ioloop.install()

    # create the server and kick it off
    parser = ZMQServer.get_options_parser()
    (options, args) = parser.parse_args()
    server = ZMQServer(options)
    server.serve()


if __name__ == '__main__':
    # dont run main() if this is a forked windows process
    if sys.modules['__main__'].__file__ == __file__:
        main()
