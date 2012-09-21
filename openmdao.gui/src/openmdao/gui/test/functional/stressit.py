import glob
import os.path
import subprocess
import sys

MAX_TRIALS = 100


def main():
    """ Run GUI functional tests up to `MAX_TRIALS` times. """
    args = ['-v']
    max_trials = MAX_TRIALS
    if len(sys.argv) > 1:
        max_trials = int(sys.argv[1])
        if max_trials < 0:
            args.append('--nonose')
            max_trials = -max_trials

    stop = 'STOP'
    if os.path.exists(stop):
        os.remove(stop)
    logfile = open('stressit.log', 'w')

    for trial in range(max_trials):
        if os.path.exists(stop):
            break
    
        avgs = os.getloadavg()
        msg = 'Trial %s: %.2f, %.2f, %.2f' \
              % (trial+1, avgs[0], avgs[1], avgs[2])
        print msg
        logfile.write('\n'+msg+'\n')

        for test_script in sorted(glob.glob('test_*.py')):
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

