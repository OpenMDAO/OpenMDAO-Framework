.. index:: Issues

Using GitHub Issues
===================


Creating a Issue
----------------
Whenever you're going to do development on the OpenMDAO code base, you should have an issue related
to that work. The issue could be related to a software requirement, or a bug that was found, or an 
enhancement that has been requested. A number of issues have already been created
for the project, but you can also create a new issue yourself.

To create a GitHub issue, follow these steps:


1.  First, log into GitHub and go to http://github.com/OpenMDAO/OpenMDAO-Framework/issues. 
    (You must be logged in to create a new issue.)

2.  Select the the **New Issue** button in the upper right hand corner. 
    
    The *New Issue* page will appear, as shown below:
    
    .. figure:: new_issue.png
       :align: center

       Creating a Issue on GitHub

    
3.  There are two sections to this page: a *Title* and a *Comments* section. Try to make the title short, but descriptive. 
    "I found a bug" is not a good title. Instead try something like "Problem with XXX when doing Y."  In the
    comments section, you should put as much detail as you can. The more information you give,  the easier it
    will be for someone to write code to address the issue you are reporting. If you can supply a small
    snippet of code that reproduces that problem, that would be ideal. You may notice that a number of
    issues have a *label* attached. Only OpenMDAO maintainers can create labels. If it is
    important that your issue has a label, you can ask one of the maintainers to label it. An issue can
    have multiple labels if they are relevant.   

4.  When you submit the issue, it will be assigned a number. If you plan to work on this issue yourself, 
    you will reference this number in your commit messages to associate your branch with the issue. 


Working on an Issue
-------------------

If you don't already have a personal fork of the OpenMDAO-Framework
repository, you should make one. Getting a personal fork of the repository
lets you have your own branches, which you can use for writing code to address
issues.
    
Start off by making a new branch for each issue you want to work. Git provides
very lightweight branches, and it's very easy to switch between them, so make
as many as you like! Once you have the branch and write a little code, you will
want to commit it. In your commit message you should reference the number of
the issue you're working on. To do that just put ``GH-???`` into your commit
message, where the ``???`` is replaced by the issue number. When you do this,
information about your branch will automatically show up on the GitHub issues
page. Now anyone who visits that page will see that you're working on it
and have some related code.

If you want to add information to the issue, you can always go back to GitHub and view the issue. On the
page for a specific issue there's a place to add comments. Comments show up underneath
the original ticket information, so scrolling down the page gives you a time history of information on the
issue. Multiple people may comment on this issue as well, so a conversation might develop around the issue.
These kinds of conversations can be very helpful. They might deal with specific implementation details, or they might
be trying to hash out what the problem really is. Adding comments is one of the ways that GitHub encourages
"social coding," and we hope you will make use of it!

.. figure:: existing_issue.png
   :align: center
 
   Viewing an Existing Issue  
       
       
When you are done with an issue, indicate this by putting ``closes GH-???`` or ``closes #???`` into your
commit message, replacing ``???`` with your issue number. Then issue a pull request to the OpenMDAO
team. When your pull request gets merged into the main repository, then the issue will be marked as closed
automatically. If you just want your change to be associated with an issue without closing it, just omit
the "closes" part.

 
