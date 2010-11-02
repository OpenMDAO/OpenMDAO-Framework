"""
Run latency & thruput tests on various server configurations.
"""

import glob
import os.path
import shutil
import time

from openmdao.main.mp_support import read_server_config
from openmdao.main.objserverfactory import connect, start_server
from openmdao.main.rbac import Credentials, set_credentials


MESSAGE_DATA = []

def init_messages():
    """ Initialize message data for various sizes. """
    for i in range(21):
        MESSAGE_DATA.append(' ' * (1 << i))


def run_test(name, server):
    """ Run latency & bandwidth test on `server`. """
    for i in range(10):
        server.echo(MESSAGE_DATA[0])  # 'prime' the connection.

    results = []
    reps = 10000
    for msg in MESSAGE_DATA:
        start = time.clock()
        for i in range(reps):
            server.echo(msg)
        et = time.clock() - start

        size = len(msg)
        latency = et / reps
        thruput = len(msg) / (et/reps)
        print '%d msgs of %d bytes, latency %g, thruput %g' \
              % (reps, size, latency, thruput)
        results.append((size, latency, thruput))

        if et > 2 and reps >= 20:
            reps /= int((et / 2) + 0.5)

    return results


def main():
    """ Run latency & thruput tests on various server configurations. """
    set_credentials(Credentials())
    init_messages()
    latency_results = {}
    thruput_results = {}

    # For each configuration...
    count = 0
    for authkey in ('PublicKey', 'UnEncrypted'):
        for ip_port in (0, -1):
            for hops in (1, 2):
                # Start factory in unique directory.
                count += 1
                name = 'Echo_%d' % count
                if os.path.exists(name):
                    shutil.rmtree(name)
                os.mkdir(name)
                os.chdir(name)
                try:
                    server_proc = start_server(authkey=authkey, port=ip_port)
                    address, port, key = read_server_config('server.cfg')
                finally:
                    os.chdir('..')

                # Connect to factory.
                print
                print '%s, %s %d, hops: %d' % (authkey, address, port, hops)
                factory = connect(address, port, authkey=authkey, pubkey=key)

                if hops == 1:
                    server = factory
                else:
                    # Create a server.
                    server = factory.create('')

                # Run test.
                results = run_test(name, server)

                # Shutdown.
                if server is not factory:
                    factory.release(server, remove_dir=False)
                factory.cleanup()
                server_proc.terminate(timeout=10)

                # Add results.
                for size, latency, thruput in results:
                    if size not in latency_results:
                        latency_results[size] = []
                    latency_results[size].append(latency)

                    if size not in thruput_results:
                        thruput_results[size] = []
                    thruput_results[size].append(thruput)

    # Write out results in X, Y1, Y2, ... format.
    header = 'Bytes,En-S-1,En-S-2,En-P-1,En-P-2,Un-S-1,Un-S-2,Un-P-1,Un-P-2\n'

    with open('latency.csv', 'w') as out:
        out.write(header)
        for size in sorted(latency_results.keys()):
            out.write('%d' % size)
            for value in latency_results[size]:
                out.write(', %g' % value)
            out.write('\n')

    with open('thruput.csv', 'w') as out:
        out.write(header)
        for size in sorted(thruput_results.keys()):
            out.write('%d' % size)
            for value in thruput_results[size]:
                out.write(', %g' % value)
            out.write('\n')

    for path in glob.glob('Echo_*'):
        shutil.rmtree(path)


if __name__ == '__main__':
    main()

