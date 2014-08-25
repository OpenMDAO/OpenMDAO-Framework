.. _`dev-checklist`:

.. _`Code-Contribution-Tutorial-&-Checklist`:

Code Contribution Tutorial & Checklist
======================================

Below is a step-by-step tutorial for you to follow if you want to contribute new code to the 
framework effort. 

* The steps below assume you have made a personal fork of the official OpenMDAO-Framework
  repository. If you have not yet done this, go to 
  https://github.com/OpenMDAO/OpenMDAO-Framework and click on the **Fork** button in the upper
  right. You should only have to do this once!   

* Additionally, if you have not done so yet, *clone* the official OpenMDAO-Framework repository. You
  should only need to do this once also. You can do it multiple times -- but you'll use a lot
  more space and waste time waiting for it to download. 

  ::

    git clone git@github.com:OpenMDAO/OpenMDAO-Framework.git  
    
         
  If you have trouble connecting, try:
	 
  ::  
    
    git clone http://github.com/OpenMDAO/OpenMDAO-Framework.git
  
* Finally, if you haven't done this already, you can save some typing by defining a remote branch in
  your local repo. Again, you have to do this just once, which is why it's included here rather than
  below. Use your GitHub username; in this example it's ``hschilling``. 

  From your local OpenMDAO-Framework repo, type: 

  ::
   
    git remote add myfork git@github.com:hschilling/OpenMDAO-Framework.git 
    
You can now follow this checklist for changing code that will get merged into the official OpenMDAO
repository (assuming the maintainers approve the change).  
  
#.  Go to the directory containing the local repository. 

    :: 
    
      cd OpenMDAO-Framework 
      
#.  The next step is to bring your local dev branch up to date with the *official* one. 

    ::
    
      git checkout dev 
      git pull origin dev

#.  Now that your local dev is up to date, you can make a new branch.

    ::  
    
      git branch <your_new_branch>
      git checkout <your_new_branch>
      
    Optionally, you can create and checkout your new branch with one command:
    
    ::
    
      git checkout -b <your_new_branch>
    

#.  If you're using the same repo you've used before, to be safe you should delete the ``devenv`` directory
    that got built the last time. 

    ::
    
      rm -rf devenv

#.  Rebuild the ``devenv`` directory with the following script.

    ::
    
      python go-openmdao-dev.py

#.  Activate the environment. 

    First, change to the ``devenv`` directory.

    ::
    
      cd devenv
    
    
    If you're on Linux, you must be running bash to activate the virtual environment. If you're not
    running it, to start it, type:

    ::
    
      bash
      
    Next, if you're on Linux or OS X, type the following:

    ::

      . bin/activate


    If you're on Windows, type:

    ::

      Scripts\activate
     
    
#.  Make changes to the code and update the tests. (For more information, see :ref:`Adding-New-Tests`.)  

#.  It's a good idea to run pylint to check for any bugs in your code. If you do not
    have pylint on your system, you can install and run it by typing:

    ::
     
      easy_install pylint  
      pylint <file_name>

    Fix any errors that pylint found.


#.  Run all the tests to make sure everything still works. (Remember that you must be in an active
    environment to run the test and docs scripts. So, if you interrupted work and logged out, be
    sure to reactivate your environment.)

    ::

      openmdao test

    Fix any errors found during testing.

#. Change the docs if needed. (For more information on creating or updating docs, see :ref:`resources`.)  

   To build and then display the docs, use the following scripts:
    
   ::
    
     openmdao build_docs
     openmdao docs


#. Test the docs. 
                
   ::
    
     openmdao test_docs
 
   Note: The doctests are automatically run whenever you run ``openmdao test``. Step 11 builds and tests
   `only` the docs.

   Fix errors if any.

#. If you have not done so, visually inspect the docs using the default browser. 
                 
            
   ::
     
     openmdao docs

#. Now merge out from the latest dev to your branch.

   ::
   
     git pull origin dev
 
#. After a successful merge out, run the full test suite again on your branch.


#. Stage the updated content for the next commit. 
                 
   ::
     
     git add .
     
   (If you've been working on your branch for any length of time, you've probably already been staging
   and committing files.)    

#. Commit the staged content. (The ``-a`` will include any changes that you forgot to explicitly add to the
   staging area with ``git add``.)   
    
   ::
    
     git commit -am "Type a short commit message identifying story or code you changed." 

 
#. Push your changes up to your personal OpenMDAO fork:

   ::
    
     git push myfork <your_branch>

#. Issue a pull request, i.e., ask the OpenMDAO maintainers to merge your changes:
 
   *  Go to your personal OpenMDAO-Framework fork on GitHub, for example:

      https://github.com/hschilling/OpenMDAO-Framework 

   *  On the left side of the page, you'll see a branch icon and the text `branch:` followed by a branch
      name. If you are not on the desired branch, click on the down arrow after the branch name to display
      a list of your branches; then select the one you want merged. 

   *  Click on the **Pull Request** button towards the top middle of the page.   

   *  Fill out the form that appears and then click the **Send pull request** button. 

      The openMDAO maintainers will be notified, and one of them will review your pull request. In the
      upper middle of the screen is a field that automatically shows your pull requests.  You should
      now show at least 1. 

#.  When you're finished, it's a good idea to deactivate your environment in case the virtualenv gets
    changed.   

    ::
    
      deactivate
