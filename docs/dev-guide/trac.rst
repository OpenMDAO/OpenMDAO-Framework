.. index:: Trac

Using Trac
===========


Creating a New Ticket
----------------------

Before you create a development branch in our Bazaar repository, you should have a ticket assigned
to you in Trac. You can create the ticket yourself, or someone on the team can assign it. 

To create a Trac ticket for yourself, follow these steps:


1.  First, go to ``http://openmdao.org/`` and log in to Trac. You must be logged in to create or edit
    a ticket.

    
2.  Select **Trac** from the menu bar and then click on **Create New Ticket.** 
    
    The *Create New Ticket* page will appear, as shown below:
    
    .. figure:: ../images/dev-guide/create_ticket.png
       :align: center
 
       Creating a ticket in Trac 
    
|
    
3.  Complete the relevant fields (which will be most of them). Be sure to assign the ticket to
    yourself using the drop-down list. 
    
    
4.  When you have completed the needed fields, click the **Create ticket** button. 

    The new ticket will be displayed with the ticket number and the information you supplied, for
    example:
    
|
    
    .. figure:: ../images/dev-guide/new_ticket.png
       :align: center
   
       New ticket with assigned number (partial page)
    
|
    
5.  To change the state from *new* to *working,* in the *Action* box at the bottom of the page,
    click on the radio button next to **work** and then click on the **Submit changes** button. 

    If you won't be working on the ticket for a while, leave it in the *new* state. 
       
    Remember to identify the ticket you are working on when you create and name your branch in the
    Bazaar repository. 


Editing a Ticket
-----------------

While working on a branch/ticket, you may want to edit the ticket for several reasons:

-  To add a comment, perhaps to document an issue or explain an implementation plan
   
-  To add a release note before proposing a branch for merge 

-  To change the status to *ready_merge* after a branch has been approved for merge

    
1.  Go to ``http://openmdao.org/`` and log in to Trac.  

2.  In the upper-right corner, enter the ticket number in the *Search* box, for example ``#470``,
    and click on the **Search** button. 

    A page showing the ticket information will appear.

3.  Change the ticket as desired. You may want to add a remark to the *Comment* field, add a brief
    statement in the *Release Notes* field, or change the state.

    If your branch has been approved for merge and you want to change the state from *working* to
    *ready_merge*, click on the radio button next to **submit_for_merge** and then click the **Submit
    changes** button.

    .. figure:: ../images/dev-guide/working.png
       :align: center
     
 
    
Viewing Tickets
----------------   

1.  To view tickets, go to ``http://openmdao.org/`` and log in to Trac. If you want to view your
    active tickets, you must be logged in, but to view tickets in general, it isn't necessary. 
    
2.  From the menu bar click on **Trac** and then on **View Tickets**.  

    You have several options for types of tickets to view. 
    
3.  If you want to see a list of available reports, click on **Available Reports** in the
    upper-right corner under the menu bar. 
    
4.  As you can see, you can also create a custom query by selecting **Custom Query** in the
    upper-right corner *under* the menu bar; or you can select **Trac-->View Tickets-->Custom Query**
    *from* the menu bar. You can add a filter and check/uncheck boxes to modify a filter.

    
    
 
