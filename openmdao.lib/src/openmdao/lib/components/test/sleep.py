import os
import sys
import time


def main():
    """ Just an external program for testing. """
    if len(sys.argv) >= 2:
        delay = float(sys.argv[1])
        if delay < 0:
            raise ValueError('delay must be >= 0')
        time.sleep(delay)

        if len(sys.argv) > 2:
            out = open(sys.argv[2], 'w')
            data = os.environ.get('SLEEP_DATA', 'no-data-available')
            out.write('%s\n' % data)
            out.close()
        inp = open('input', 'rU')
        out = open('output', 'w')
        out.write(inp.read())

        if os.environ.get('SLEEP_ECHO'):
            data = sys.stdin.read()
            sys.stdout.write('stdin echo to stdout\n')
            sys.stdout.write(data)
            sys.stderr.write('stdin echo to stderr\n')
            sys.stderr.write(data)

        sys.exit(0)
    else:
        print "usage: python sleep.py nseconds [dummyfile]"
        sys.exit(1)


if __name__ == '__main__': # pragma no cover
    main()

