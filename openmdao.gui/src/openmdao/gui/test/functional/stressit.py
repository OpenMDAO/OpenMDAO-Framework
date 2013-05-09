import glob
import optparse
import os.path
import subprocess
import sys


def main():
    """ Repeatedly run GUI functional tests. """
    parser = optparse.OptionParser()
    parser.add_option('-t', '--trials', type='int', default=100,
                      help='# trials to attempt')
    parser.add_option('-n', '--nonose', action='store_true',
                      help='if present, run outside of nose')
    options, files = parser.parse_args()

    args = ['-v']
    if options.nonose:
        args.append('--nonose')

    for stop in ('STOP', 'STOP.txt'):
        if os.path.exists(stop):
            os.remove(stop)
    logfile = open('stressit.log', 'w')

    for trial in range(options.trials):
        if os.path.exists('STOP') or os.path.exists('STOP.txt'):
            break
    
        if sys.platform == 'win32':
            msg = 'Trial %s:' % (trial+1)
        else:
            avgs = os.getloadavg()
            msg = 'Trial %s: %.2f, %.2f, %.2f' \
                  % (trial+1, avgs[0], avgs[1], avgs[2])
        print msg
        logfile.write('\n'+msg+'\n')

        if not files:
            files = sorted(glob.glob('test_*.py'))

        for test_script in files:
            msg = '    Running %s' % test_script
            print msg
            logfile.write(msg+'\n')
            logfile.flush()
            cmd = ['python', test_script]
            cmd.extend(args)
            status = subprocess.call(cmd, stdout=logfile, stderr=subprocess.STDOUT)
            if status:
                msg = '        exit status %s' % status
                print msg
                logfile.write(msg+'\n')
                sys.exit(status)

    logfile.close()


if __name__ == '__main__':
    main()

