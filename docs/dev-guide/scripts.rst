
Remote Testing Tools
====================

The ``openmdao.devtools`` package contains a number of console scripts that
allow openmdao to be tested and built on remote hosts. This section describes
how to setup and use the scripts.

General Setup
-------------

Information about remote hosts is contained in a config file.  An example
of such a file is ``config/testhosts.cfg`` in the 
OpenMDAO-Framework repository.  This file should be copied to
``~/.openmdao/testhosts.cfg`` and modified to contain the hosts or ec2 images
you intend to test on.  The scripts look for this file in ``~/.openmdao``
by default.  You can specify a different config file on the command line using
the ``-c`` argument.

Aside from the [DEFAULT] section, the file has one section per 
host or EC2 image.  The section name is used as a short alias for that host 
and is used with the --host=<section_name> arg in the testing scripts.


EC2 Specific Setup
------------------

To run the scripts on EC2 images or non-running instances, you must create
a ``~/.boto``  config file with the appropriate id and secret key.  You may
also specify other information in the .boto file, e.g., debug level.  An
example of a ``.boto`` file is shown below.


::

    [Credentials]
    aws_access_key_id = <your id here>
    aws_secret_access_key = <your secret key here>
    
    [Boto]
    debug = 0
    num_retries = 5
    
    #proxy = myproxy.com
    #proxy_port = 8080
    #proxy_user = <your proxy userid>
    #proxy_pass = <your proxy password>


SSH keys
~~~~~~~~

You'll need an identity file to execute operations like starting and
stopping instances on EC2 using the *boto* package. For openmdao
we use an identity file called ``lovejoy.pem`` for all of our EC2 images
and instances. The identity file should be placed in the ``~/.ssh`` directory.

In order to actually connect to a given host via SSH, you'll need to take
your personal public key for the host your're connecting from and put it
in the authorized_keys file on the destination host.  This is true whether
the host is an EC2 host or not.


Scripts
-------

The following section describes each script in detail. All scripts accept the
**-h** and the **--help** command line options which will display all of their
allowed arguments.


test_branch
~~~~~~~~~~~

The *test_branch* script is used to test a branch using *openmdao_test*
run on a group of remote hosts. Running it with
a **-h** option shows the following:

::

    Usage: test_branch [OPTIONS] -- [options to openmdao_test]

    Options:
       -h, --help     show this help message and exit
       -c CONFIG, --config=CONFIG
                      Path of config file where info for hosts is located
       --host=HOST    Select host from config file to run on. To run on
                      multiple hosts, use multiple --host args.
       --all          If True, run on all hosts in config file.
       -o OUTDIR, --outdir=OUTDIR
                      Output directory for results (defaults to ./host_results)
       -k, --keep     If there are test/build failures, don't delete the
                      temporary build directory. If testing on EC2, stop 
                      the instance instead of terminating it. 
       -f FNAME, --file=FNAME
                      Pathname of a tarfile or URL of a git repo. 
                      Defaults to the current repo.
       -b BRANCH, --branch=BRANCH
                      If file is a git repo, supply branch name here


The tests run concurrently and write their outputs to 
<outdir>/<host_config_name>/run.out where *outdir* defaults to ``host_results``
and *host_config_name* is the section name for that host in the config file.

The --host arg can be used multiple times in order to specify more than one
host.

The script can test the current (committed) branch of a git repository, 
a tarred repository, or a specific branch of a specified local or remote git 
repository.  If a git repository is specified rather than a tar file, then
the branch must also be specified.  If no **-f** is supplied, the current
branch of the current repository is used.

If a **--** arg is supplied, any args after that are passed to openmdao_test
on the remote host.  Adding a **-x** arg after the **--** arg, for example, 
would cause the test to end as soon as any test on the remote host failed.
Adding the name of a specific module to test can also be a big time saver
when debugging a specific test failure.


test_release
~~~~~~~~~~~~

The test_release script is used to test a release using *openmdao_test*
run on a group of remote hosts.  It can also be used to test an existing 
production release on a specific host. Running it with a **-h** option 
shows the following:


::

    Usage: test_release [OPTIONS] -- [options to openmdao_test]

    Options:
      -h, --help        show this help message and exit
      -c CONFIG, --config=CONFIG
                        Path of config file where info for hosts is located
      --host=HOST       Select host from config file to run on. To run on
                        multiple hosts, use multiple --host args
      --all             If True, run on all hosts in config file.
      -o OUTDIR, --outdir=OUTDIR
                        Output directory for results (defaults to
                        ./host_results)
      -k, --keep        Don't delete the temporary build directory. If testing
                        on EC2 stop the instance instead of terminating it.
      -f FNAME, --file=FNAME
                        URL or pathname of a go-openmdao.py file or pathname
                        of a release dir

The **-f** argument is used to specify either the ``go-openmdao.py`` file that 
builds the release environment, or the path to a directory that was built 
using the *make_release* script.


Release Building and Publishing
===============================

After branch testing is complete, it may be time to create a new OpenMDAO
release. The tools and procedures described below make the process a little
easier.


Release Creation
----------------

The *make_release* script is used to build the required distribution tar
files for all of the OpenMDAO packages. It also builds the html version
of the docs and the go-openmdao.py bootstrapping installer file.  
Running *make_release* with a **-h** option shows the following:

::

    Usage: make_release [options]

    Options:
      -h, --help        show this help message and exit
      -d DESTDIR, --destination=DESTDIR
                        directory where distributions and docs will be placed
      -v VERSION, --version=VERSION
                        version string applied to all openmdao distributions
      -m COMMENT        optional comment for version tag
      -b BASE, --basebranch=BASE
                        base branch for release. defaults to master
      -t, --test        used for testing. A release branch will not be created
      -n, --nodocbuild  used for testing. The docs will not be rebuilt if they
                        already exist
      --host=HOST       host from config file to build bdist_eggs on. Multiple
                        --host args are allowed.
      -c CONFIG, --config=CONFIG
                        path of config file where info for hosts is located


The script places all of the tar files and docs in the destination directory
specified with the **-d** option. The version number is specified with **-v**
and must be later than any version already existing on openmdao.org. OpenMDAO
releases require binary distributions on Windows for certain packages, so
*make_release* will fail if you don't specify a Windows host using the
**--host** option. The **-t** and **-n** options should only be used for
testing purposes.


Release Testing
~~~~~~~~~~~~~~~

See the previous description of *test_release*.


Making an Official Release
~~~~~~~~~~~~~~~~~~~~~~~~~~

Once all of the distribution packages have been made and the release has 
been tested on all platforms of interest, it's time to make it official
by pushing it up to the distribution area on openmdao.org using the
*push_release* script as follows:

::

    push_relase <release_directory> http://openmdao.org

where *release_directory* is the destination directory you supplied earlier
when you called *make_release*.  The *push_release* script takes the files
in the release directory and places them in the proper locations on the
server, i.e. the docs and the go-openmdao.py file go in the *downloads* 
area and the distribution packages go in the *dists* area.  The second
argument to *push_release* can be the URL of a different server or even
a local directory path if you need to debug or test the process outside
of the production environment.  This is actually what *test_release* does
when you supply it with a release directory.

The last step is to update the repository on github...
