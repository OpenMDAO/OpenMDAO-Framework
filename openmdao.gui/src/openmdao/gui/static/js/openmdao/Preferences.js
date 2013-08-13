/***********************************************************************
 * Preferences: User preferences for various aspects of the GUI.
 *
 * TODO: Just a place to store default preferences for now.
 *       Implementation of full preference subsystem is future work.
 ***********************************************************************/

openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

// initialize preferences (TODO: persistent prefs, per user and per project)
openmdao.preferences = {
    FileTreeFrame: {
        highlightOnUpdate: false    // highlight the file tree when it is updated
    },

    ObjectTreeFrame: {
        select: 'Workflow'          // which tree to select (Workflow or Components)
    },

    PropertiesPane: {               // multiple entries, per object name
    },

    SlotFigure: {
        resize: true                // resize figure width to accommodate long name
    }
};
