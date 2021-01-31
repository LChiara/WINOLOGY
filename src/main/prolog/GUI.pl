/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  GUI.pl

 Description:
 Implements the user interface, that allows the user to add a new entry
 in the database and classify a new wine.

*/


/* Imports */
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).

:- ensure_loaded(characteristics).
:- ensure_loaded(classify).
:- ensure_loaded(register).


/* Define resources path, where image are to be found */
:- pce_image_directory('../resources/img').

resource(title, title, image('title.jpg')).
resource(regis, regis, image('images.jpg')).
resource(check, check, image('match.jpg')).


/* startGUI
 * create the main window of the GUI
 */

startGUI :-
    new(Menu, window('WINOLOGY', size(800,600))),
    showMenu(Menu).

/* Release resources */
free_main:-
    free(@title),
    free(@regis),
    free(@check),
    free(@register_label),
    free(@check_label),
    free(@search_wine),
    free(@ok_button),
    free(@ok),
    free(@cancel_button).

free_dialogs :-
    free(@entry_name),
    free(@entry_time),
    free(@entry_food),
    free(@entry_person),
    free(@entry_rating),
    free(@entry_aroma),
    free(@entry_body),
    free(@entry_effervescence),
    free(@entry_color),
    free(@entry_sweetness),
    free(@ok_button),
    free(@cancel_button),
    free(@search_wine),
    free(@container_entries),
    free(@container_buttons),
    free(@result_rating).

free_ratings :-
    free(@entry_rating).


/*
 * add_image(+Window, +Figure, +Image, +Position).
 * Method pull from internet to add an image insiede a window.
 */
add_image(Window, Figure, Image, Position) :-
    new(Figure, figure),
    new(Bitmap, bitmap(resource(Image), @on)),
    send(Bitmap, name, 1),
    send(Figure, display, Bitmap),
    send(Figure, status, 1),
    send(Window, display, Figure, Position).

/* DialogWindows < Dialog
 *  Small class (child of built-in xpce class *dialog*)
 that creates a dialog where the user can choose all the characteristics
 about a rating and the related wine
 */

:- pce_begin_class(dialogWindow, dialog).

variable(result, name*, get, "Result from Prolog Callback").

initialise(DialogWindow) :->
    "Initialise method"::
    send(DialogWindow, send_super, initialise, 'Dialog'),

    % free all variables
    free_dialogs,
    free_ratings,

    % create container for entries on the left.
    new(@container_entries, dialog_group(' ')),
    send(DialogWindow, append, @container_entries),
    send_list(@container_entries, append,
              [new(@entry_name, text_item(name)),
               new(@entry_time, menu(time, cycle)),
               new(@entry_food, menu(food, cycle)),
               new(@entry_person, menu(person, cycle)),
               new(@entry_aroma, menu(aroma, cycle)),
               new(@entry_body, menu(body, cycle)),
               new(@entry_color, menu(color, cycle)),
               new(@entry_effervescence, menu(effervescence, marked)),
               new(@entry_sweetness, menu(sweetness, cycle))]),

    % Create all the entries,
    % filling up the combo boxes with the related possible characteristics
    maplist(createEntry,
            [@entry_time, @entry_food, @entry_person,
             @entry_aroma, @entry_body, @entry_color, @entry_effervescence, @entry_sweetness],
            [time, food, person, aroma, body, color, effervescence, sweetness]).
:- pce_end_class.

/* showMenu(+Menu)
 *  create and display the main window (image and buttons).
 */
showMenu(Menu):-

    % free variables
    free_main,

    % add title image
    add_image(Menu, @title, title, point(150, 20)),

    % add button for "REGISTER" and related action to open a new dialog.
    add_image(Menu, @regis, regis, point(250, 430)),
    new(@register_label, text('Register')),
    new(_, constraint(@regis, @register_label, identity(center_x))),
    send(@regis, recogniser,
         click_gesture(left, '', single,
                       message(@prolog, registerAction))),
    send(Menu, display, @register_label, point(250, 430+64+10)),

    % add button for "CLASSIFICATION" and related action to open a new dialog.
    add_image(Menu, @check, check, point(350, 430)),
    new(@check_label, text('Check')),
    new(_, constraint(@check, @check_label, identity(center_x))),
    send(@check, recogniser,
         click_gesture(left, '', single,
                       message(@prolog, classifyAction))),
    send(Menu, display, @check_label, point(500, 430+64+10)),

    % open the Menu in central position.
    send(Menu, open_centered).



/* -> BEGIN Action section */

/* registerAction.
 * prompt the register dialog where the user can enter all the necessary
 * info and add it to the DB calling the related register function
 */
registerAction :-
    promptRegisterDialog(Rating, RatingEntryValues, WineName, WineEntryValues),
    createEntries(Rating, RatingEntryValues, WineName, WineEntryValues),
    register(Rating, RatingEntryValues, WineName, WineEntryValues).

/* classifyAction.
 * prompt the classify dialog where the user can enter all the necessary
 *info, start the learning process and classify the entry
 */
classifyAction :-
    promptClassifyDialog(EntryValues),
    !,
    startLearningProcess,
    createRequest(EntryValues, SituationDescription),
    writeln("Result ==> "),
    classify(SituationDescription, RatingClass),
    writeln(RatingClass),
    promptRatingResult(SituationDescription, RatingClass).

/* -> END Action section */

/* -> BEGIN Dialog section */

/* promptRegisterDialog(?Rating, ?RatingEntryValues, ?Name, ?WineEntryValues)
 *  create and display the register dialog, where the user can enter all
 *  the necessary information, returning the rating class, the rating
 *  entry, the wine name and the wine entry.
 */
promptRegisterDialog(Rating, RatingEntryValues, Name, WineEntryValues):-
    new(Dialog, dialogWindow),

    send(@container_entries, append,
         new(@entry_rating, menu(rating, cycle)), below),

    new(@container_buttons, dialog_group(buttons, group)),
    send(Dialog, append, @container_buttons, right),
    send(@container_buttons, append,
         new(@search_wine, button(search_wine, message(@prolog, searchWineInDB,
                                                                 @entry_name?selection,
                                                                 @entry_aroma,
                                                                 @entry_body,
                                                                 @entry_color,
                                                                 @entry_effervescence,
                                                                 @entry_sweetness)))),
    send(@container_buttons, append,
         new(@ok_button, button(ok_button, message(Dialog, return,
                                                 [@entry_name?selection,
                                                  @entry_time?selection,
                                                  @entry_food?selection,
                                                  @entry_person?selection,
                                                  @entry_rating?selection,
                                                  @entry_aroma?selection,
                                                  @entry_body?selection,
                                                  @entry_color?selection,
                                                  @entry_effervescence?selection,
                                                  @entry_sweetness?selection]))), below),
    send(@container_buttons, append,
         new(@cancel_button, button(cancel_button, message(Dialog, return, @nil))), below),


    characteristic(rating, RatingClass),
    send_list(@entry_rating, append, RatingClass),
    send(Dialog, default_button, @ok_button),
    get(Dialog, confirm, Answer),
    send(Dialog, destroy),
    Answer \== @nil,
    get(@entry_name, selection, Name),
    get(@entry_time, selection, Time),
    get(@entry_food, selection, Food),
    get(@entry_person, selection, Person),
    get(@entry_rating, selection, Rating),
    get(@entry_aroma, selection, Aroma),
    get(@entry_body, selection, Body),
    get(@entry_color, selection, Color),
    get(@entry_effervescence, selection, Effervescence),
    get(@entry_sweetness, selection, Sweetness),
    RatingEntryValues = [
                (   name, Name),
                (   time, Time),
                (   food, Food),
                (   person, Person),
                (   rating, Rating)],
    WineEntryValues = [
                (   aroma, Aroma),
                (   body, Body),
                (   color, Color),
                (   effervescence, Effervescence),
                (   sweetness, Sweetness)],
    writeln(RatingEntryValues),
    writeln('\n'),
    writeln(WineEntryValues).

/* promptClassifyDialog(?RequestList)
 *  create and display the classify dialog, where the user can enter all
 *  the necessary information about the wine to classify, returning a list
*/
promptClassifyDialog(RequestList) :-
    new(Dialog, dialogWindow),

    % Append empty value to combo boxes.
    maplist(allowEmpty,
            [@entry_time, @entry_food, @entry_person,
             @entry_aroma, @entry_body, @entry_color, @entry_effervescence, @entry_sweetness]),

    % Create button container (search wine, ok, cancel).
    new(@container_buttons, dialog_group(buttons, group)),
    send(Dialog, append, @container_buttons, right),
    send(@container_buttons, append,
         new(@search_wine, button(search_wine, message(@prolog, searchWineInDB,
                                                                 @entry_name?selection,
                                                                 @entry_aroma,
                                                                 @entry_body,
                                                                 @entry_color,
                                                                 @entry_effervescence,
                                                                 @entry_sweetness)))),
    send(@container_buttons, append,
         new(@ok_button, button(ok_button, message(Dialog, return,
                                                 [@entry_name?selection,
                                                  @entry_time?selection,
                                                  @entry_food?selection,
                                                  @entry_person?selection,
                                                  @entry_aroma?selection,
                                                  @entry_body?selection,
                                                  @entry_color?selection,
                                                  @entry_effervescence?selection,
                                                  @entry_sweetness?selection]))), below),
    send(@container_buttons, append,
         new(@cancel_button, button(cancel_button, message(Dialog, return, @nil))), below),
    send(Dialog, default_button, @ok_button),

    % return requested values and close dialog.
    get(Dialog, confirm, Answer),
    send(Dialog, destroy),
    Answer \== @nil,
    get(@entry_time, selection, Time),
    get(@entry_food, selection, Food),
    get(@entry_person, selection, Person),
    get(@entry_aroma, selection, Aroma),
    get(@entry_body, selection, Body),
    get(@entry_color, selection, Color),
    get(@entry_effervescence, selection, Effervescence),
    get(@entry_sweetness, selection, Sweetness),

    RequestList = [ (   time, Time),
                (   food, Food),
                (   person, Person),
                (   aroma, Aroma),
                (   body, Body),
                (   color, Color),
                (   effervescence, Effervescence),
                (   sweetness, Sweetness)].

promptRatingResult(Request, Result) :-
    new(D, dialog('Rating')),
    atom_concat('Rating: ', Result, RatingResult),
    send_list(D, append,
              [new(_, text(Request, center, normal)),
              new(_, text(RatingResult, center, normal)),
              new(@ok, button(ok, message(D, destroy)))]),
    send(D, open_centered).

/* -> END Dialog section */

/* -> BEGIN Utility function section */

/* createEntry(+DialogEntry, +Name)
 *  Utility function to create a combo box, adding all possible entry
 *  values.
 */
createEntry(DialogEntry, Name) :-
    characteristic(Name, Value),
    send_list(DialogEntry, append, Value).

/* allowEmpty(+DialogEntry)
 *  Utility function to add possibility to select an empty value.
 */
allowEmpty(DialogEntry) :-
    constant(empty_value, EmptyValue),
    send(DialogEntry, append, EmptyValue),
    send(DialogEntry, default, EmptyValue).


/* searchWineInDB(+WineName, ?AromaEntry, ?BodyEntry, ?ColorEntry, ?EffervescenceEntry, ?SweetnessEntry)
 * search a specific wine in the database, if it already exists, entries
 * will be filled with the right value.
 */
searchWineInDB(WineName, AromaEntry, BodyEntry, ColorEntry, EffervescenceEntry, SweetnessEntry) :-
    get_wine(wineDescription(WineName, [aroma=Aroma, body=Body, color=Color, effervescence=Effervescence, sweetness=Sweetness])),
    write('Searched wine => '),
    writeln(WineName),
    write('aroma='),
    writeln(Aroma),
    write('body='),
    writeln(Body),
    write('color='),
    writeln(Color),
    write('effervescence='),
    writeln(Effervescence),
    write('sweetness='),
    writeln(Sweetness),
    send(AromaEntry, selection, Aroma),
    send(BodyEntry, selection, Body),
    send(ColorEntry, selection, Color),
    send(EffervescenceEntry, selection, Effervescence),
    send(SweetnessEntry, selection, Sweetness).
searchWineInDB(_, _, _, _, _, _) :- !.

/* -> End Utility function section */

createEntries(_Rating, RatingEntryValues, _WineName, WineEntryValues):-
    createAtom(RatingEntryValues, RegisterEntryAtom),
    writeln(RegisterEntryAtom),
    createAtom(WineEntryValues, WineEntryAtom),
    writeln(WineEntryAtom).

createRequest(EntryValues, Request) :-
    createAtom(EntryValues, Request),
    write('User Request: '),
    writeln(Request).














