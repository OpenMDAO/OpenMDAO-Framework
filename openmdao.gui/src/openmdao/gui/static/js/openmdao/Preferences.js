/***********************************************************************
 * Preferences: User preferences for various aspects of the GUI.
 *
 * TODO: Just a place to store default preferences for now.
 *       Implementation of full preference subsystem is future work.
 ***********************************************************************/

openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

// initialize preferences (TODO: persistent prefs, per user and per model)
openmdao.preferences = {
    PropertiesPane: {   // multiple entries, per object name
    },

    SlotFigure: {
        resize: false   // resize figure width to accommodate long name
    }
};
