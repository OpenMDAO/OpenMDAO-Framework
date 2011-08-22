
Remote Testing Tools
====================

The ``openmdao.devtools`` package contains a number of console scripts that
allow openmdao to be tested and built on remote hosts. This section describes
how to setup and use the scripts.

Setup
-----

Information about remote hosts is contained in a config file.  An example
of such a file is ``config/default_testing_config`` which can be found in the 
OpenMDAO-Framework repository.  This file should be copied into your home
directory as ~/.openmdao/testhosts.cfg.  The scripts look for this file
by default.  You can also specify a config file on the command line using
the ``-c`` argument.

Aside from the [DEFAULT] section, the config file has one section per 
host or EC2 image.  The section name is used as a short alias for that host 
and is used with the --host=<section_name> arg in the scripts.

sections for EC2 images have
image_id
instance_type  (machine size)
user  (username)
platform  (linux, windows, or osx)
sections for EC2 instances have
platform
user
instance_id
sections for generic hosts have
platform
user
addr  (ip address)

EC2 Specific
create ~/.boto  config file with id and secret key, other boto config (debug, etc.)
get identity file lovjoykey.pem and save in ~/.ssh
connect using ‘ssh -i <identity> -l <username> <hostname>



Running dev tests
test_branch [options] -- [options passed to openmdao_test on remote end]
options:
-c, --config:  sets config file
--host: specifies a host using section name from config file
-k, --keep: won’t delete remote directories
-o, --outdir: specify a directory where all test_branch results will go
-f, --file: specify tar file of repo or url of git repo
tests run concurrently and write their outputs to <outdir>/<host_config_name>/run.out
can test current branch of a git repo, a tarred repo, or a specific branch of a specified local or remote git repo


Running release tests (assumes branch testing has already happened)
make_release -v <version> -d <dest> --host=<bin_host>
supply a --host arg for each platform where binaries are needed
push_relase <src> <dest>
takes files created in make_release and puts them in the proper directory structure.
dest can be a URL
they can be pushed to a new directory or to an existing release location
test_release -f <gofile or release dir> --host=xxx --host=yyy
gofile must be a URL or must live in a properly structured release directory
release dir will be copied to host and downloads/latest/go-openmdao.py will be used for testing
if test_release succeeds...
push_release to production area