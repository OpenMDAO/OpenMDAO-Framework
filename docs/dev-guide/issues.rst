.. index:: Issues

.. _`github_issues`:

Using GitHub Issues
===================


Creating a Issue
----------------
If you are not on the OpenMDAO development team and you want to submit a bug or request an 
enhancement, you should submit an issue in GitHub. 

To create a GitHub issue, follow these steps:


1.  First, log into GitHub and go to https://github.com/OpenMDAO/OpenMDAO-Framework/issues. 
    (You must be logged in to create a new issue.)

2.  Select the the **New Issue** button in the upper right hand corner. 
    
    The *New Issue* page will appear, as shown below:
    
    .. figure:: new_issue.png
       :align: center

       Creating a Issue on GitHub

    
3.  There are two sections to this page: a *Title* and a *Comments* section. Try to make the title short, but
    descriptive.  "I found a bug" is not a good title. Instead try something like "Problem with XXX when doing
    Y."  In the comments section, you should put as much detail as you can. The more information you give, 
    the easier it will be for someone to write code to address the issue you are reporting. If you can supply
    a small snippet of code that reproduces that problem, that would be ideal. You may notice that a number of
    issues have a *label* attached. Only OpenMDAO maintainers can create labels. If it is important that your
    issue has a label, you can ask one of the maintainers to label it. 

4.  When you submit the issue, it will be assigned a number. This number can be referenced in the commit
    messages to associate the branch where it is being worked with the issue. 


Working on an Issue
-------------------

If you don't already have a personal fork of the OpenMDAO-Framework repository, you should make one.
Getting a personal fork of the repository lets you have your own branches, which you can use for
writing code to address issues.

Start off by making a new branch for each issue you want to work. Git provides very lightweight
branches, and it's very easy to switch between them, so make as many as you like! Once you have the
branch and write a little code, you will want to commit it. If you are working on an issue in
GitHub, ideally you would reference the issue number in your commit message. To do that just put
``GH-###`` in your commit message, where the ``###`` is replaced by the issue number. When you do
this, information about your branch will automatically show up on the GitHub issues page. Now anyone
who visits that page will see that you're working on it.

If you want to add information to the issue, you can always go back to GitHub, view the issue, and 
add a comment. Comments show up underneath the original ticket information, so scrolling down the
page gives you a time history of information on the issue. Multiple people may comment on this issue
as well, so a conversation might develop around the issue. These kinds of conversations can be very
helpful. They might deal with specific implementation details, or they might be trying to hash out
what the problem really is. Adding comments is one of the ways that GitHub encourages "social
coding," and we hope you will make use of it!

.. figure:: existing_issue.png
   :align: center
 
   Viewing an Existing Issue  
       
       
When you are done with an issue, indicate this by putting ``closes GH-###`` or ``closes ###`` in
your commit message, replacing ``###`` with your issue number. Then issue a pull request to the
OpenMDAO team. The number of the closed issue should automatically appear in your pull request
title. When your pull request gets merged into the main repository, the issue will automatically be
marked as closed. If, however, you just want your change to be associated with an issue without
closing it, just omit the "closes" part.

Tracking Bug/Feature Development in Pivotal Tracker
====================================================

While GitHub allows you to submit and then close an issue you worked on, it does not show you which
features are currently being worked on by the OpenMDAO dev team or how the project is progressing.
OpenMDAO uses Pivotal Tracker, a project management tool designed for agile software development, to
track all code development. As users you do not have direct access to the OpenMDAO-Framework project
in Pivotal Tracker; however, since the project is public, you can view project information to see the
status of features being developed.
 

In Pivotal Tracker code changes are submitted as `stories`. A story can be a bug fix, a new feature,
or a chore affecting project management. It might even be an user-submitted issue entered into Pivotal
Tracker by one of the OpenMDAO devs. As you can see in the example below, stories in the first column
are DONE. Releases also show up in this column. The middle column, CURRENT, shows stories currently
being worked on by the dev team, and the last column, BACKLOG, lists stories waiting to be worked on.
Stories in this column have been discussed and prioritized, so if a developer finishes a story, he can
select one from the BACKLOG. When he clicks on the `Start` button, the story is assigned to him and
moves to the CURRENT column.  

.. figure:: pivotal_tracker.png
   :align: center
 
   Example of OpenMDAO Story Tracking in Pivotal Tracker

When you go to the OpenMDAO-Framework project in Pivotal Tracker, what you see will probably be
different from the above example. For instance, you might see just the BACKLOG and the ICEBOX columns.
(The ICEBOX is a list of all stories in the system that have not yet been prioritized. These stories
will eventually move to the BACKLOG.) To see completed stories, select the MORE tab at the top of the
page (under the project name) and click *Done* on the drop-down menu. The DONE column will then
appear showing you the list of completed stories.

So, if you wish to check the status of code development in OpenMDAO, just follow this link to
the OpenMDAO-Framework project in Pivotal Tracker:  https://www.pivotaltracker.com/s/projects/470293. 

