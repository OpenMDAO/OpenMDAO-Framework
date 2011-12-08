Developer Checklist
===================

Below is a checklist, or end-to-end example, for you to follow as you change code
that will be contributed to the OpenMDAO-Framework repository. 

* The steps below assume you have made a personal fork of the official OpenMDAO-Framework
  repository. If you have not yet done this, go to 
  https://github.com/OpenMDAO/OpenMDAO-Framework and click on the **Fork** button in the upper
  right. You should only have to do this once!   

* Also, if you have not done so yet, *clone* the official OpenMDAO-Framework repository. You
  should really only need to do this once. You can do it multiple times--but you'll use a lot
  more space and waste time waiting for it to download. 

  ::

    git clone git@github.com:OpenMDAO/OpenMDAO-Framework.git  
  
* Finally, if you haven't done this already, you can save some typing by defining a remote branch in
  your local repo. Again, you have to do this just once, which is why it's included here rather than
  below. Use your GitHub username; in this example it's ``hschilling``. 

  From your local OpenMDAO-Framework repo, type: 

  ::
   
    git remote add myfork git@github.com:hschilling/OpenMDAO-Framework.git 
    
You can now follow the checklist for changing code that will get merged into the official OpenMDAO
repository (assuming the maintainers approve the change).  
  
1.  Create an issue on GitHub. 

    Log into GitHub.
    
    Go to the issues page: https://github.com/OpenMDAO/OpenMDAO-Framework/issues 
    
    Select the **New Issue** button and fill out the form. (Some issues are labeled. Only users with admin rights can label an issue.) 
    
    Click on the **Submit new issue** button when finished. Remember the issue number that is assigned. For
    this example, assume it is 529. 

2.  Go to the directory containing the local repository. 

    :: 
    
      cd OpenMDAO-Framework 
      
3.  The next step is to bring your local dev branch up to date with the *official* one. 

    ::
    
      git checkout dev 
      git pull origin dev

4.  Now that your local dev is up to date, you can make a new branch.

    ::  
    
      git branch 529-fix_bug
      git checkout 529-fix_bug

5.  If you're using the same repo you've used before, to be safe you should delete the ``devenv`` directory
    that got built the last time. 

    ::
    
      rm -rf devenv

6.  Rebuild the ``devenv`` directory with the following script.

    ::
    
      python go-openmdao-dev.py

7.  Activate the environment.

    ::
    
      cd devenv
      ./bin/activate


8.  Make changes to code and update the tests. (For more information, see :ref:`Adding-New-Tests`.)  

9.  It's a good idea to run pylint to check for any bugs in your code. If you do not
    have pylint on your system, you can install and run it by typing:

    ::
     
      easy_install pylint  
      pylint <file_name>

    Fix any errors that pylint found.


10. Run all the tests to make sure everything still works. (Remember that you must be in an active
    environment to run the test and docs scripts. So, if you interrupted work and logged out, be
    sure to reactivate your environment.)

    ::

      openmdao test

    Fix any errors found during testing.

11. Change the docs if needed. (For more information on creating or updating docs, see
    :ref:`resources`.)  

    To build and then display the docs, use the following scripts:
    
    ::
    
      openmdao build_docs
      openmdao docs


12. Test the docs. 
                
    ::
    
      openmdao test_docs
 
    Note: The doctests are automatically run whenever you run ``openmdao test``. Step 11 builds and tests
    `only` the docs.

    Fix errors if any.

13. If you have not done so, visually inspect the docs using the default browser. 
                 
            
    ::
     
      openmdao docs

14.  Stage the updated content for the next commit.
                 
     ::
     
       git add .

15. Commit the staged content. (The ``-a`` will include any changes that you forgot to explicitly add to the
    staging area with ``git add``.) Use the issue number from Step 1 in your comment. 
    
    ::
    
      git commit -am "closes GH-529: Changes to support non-rst files in plugin docs" 

 
16. Push your changes up to your personal OpenMDAO fork:

    ::
    
      git push myfork 529-fix_bug

17. Ask the OpenMDAO maintainers to merge your changes (issue a pull request).
 
*  Go to your personal OpenMDAO-Framework fork on GitHub. 

   https://github.com/hschilling/OpenMDAO-Framework

*  Use the **Switch Branches** menu (upper left) to select branch ``529-fix_bug``

*  Click the **Pull Request** button in the upper right corner. 
 
*  Fill out the form that appears and click the **Send pull request** button.

*  When your pull request gets merged into the main repository, then issue 529 will get closed automatically.

