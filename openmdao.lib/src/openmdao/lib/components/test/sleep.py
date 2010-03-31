import os
import sys
import time


def main():
    """ Just an external program for testing. """
    if len(sys.argv) >= 2:
        time.sleep(float(sys.argv[1]))
        if len(sys.argv) > 2:
            out = open(sys.argv[2], 'w')
            data = os.environ.get('SLEEP_DATA', 'no-data-available')
            out.write('%s\n' % data)
            out.close()
        sys.exit(0)
    else:
        print "usage: python sleep.py nseconds [dummyfile]"
        sys.exit(1)


if __name__ == '__main__':
    main()

