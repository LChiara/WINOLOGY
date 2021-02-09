/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  register.pl

 Description:
 Implements the register function in order to add a new entry in the
 wine and rating database, building the needed atom to create a fact
 that can be asserted in the related database.

*/

/* Imports */
:- ensure_loaded(module_db_rating).
:- ensure_loaded(module_db_wine).
:- ensure_loaded(constants).

/* register(+Rating, +RatingEntry, +WineName, +WineEntry).
 *  Register a new Entry in DB for rating and for Wine
 */
register(Rating, RatingEntry, WineName, WineEntry) :-
    registerRatingInDB(Rating, RatingEntry),
    registerWineInDB(WineName, WineEntry),
    !.

/* registerWineInDB(+WineName, +Entry)
 * Register the wine entry if the wine is not yet in the DB.
 */
registerWineInDB(WineName, Entry) :-
    constant(db_wine, File),
    attach_wine_db(File),
    createAtom(Entry, WineDescription),
    \+ get_wine(wineDescription(WineName, [WineDescription])),
    !,
    set_wine(wineDescription(WineName, [WineDescription])),
    detach_wine_db.

/* registerRatingInDB(+Rating, +Entry)
 *  Register the wine entry if the wine is not yet in the DB.
 */
registerRatingInDB(Rating, Entry) :-
    constant(db_rating, File),
    attach_rating_db(File),
    createAtom(Entry, RatingDescription),
    set_rating(rating(Rating, [RatingDescription])),
    detach_rating_db.

/* createAtom(+List, ?Output).
 *  Create an atom, joining a list of tuple values (key, value) and
 *  ignoring tuples with value=emptyValue
 */
createAtom([(_, Value)|T], OutputAtom) :- %Start: OutputAtom is still empty
    constant(empty_value, Value),
    createAtom(T, OutputAtom).

createAtom([(Key, Value)|T], OutputAtom) :- %Start: OutputAtom is still empty
    concatenate([Key, '=', Value], ActualAtom),
    createAtom(T, ActualAtom, OutputAtom).

createAtom([], ActualAtom, ActualAtom) :- !.

createAtom([(_, Value)|T], ActualAtom, Atom) :- %Skip entries empty value.
    constant(empty_value, Value),
    createAtom(T, ActualAtom, Atom).

createAtom([(Key, Value)|T], ActualAtom, Atom) :-
    createAtom(T, ActualAtom, Tmp),
    concatenate([Tmp, ', ', Key, '=', Value], Atom).


/* concatenate(+StringList, ?StringResult).
 * Concatenate a list of string (method pulled from internet).
 */
concatenate(StringList, StringResult) :-
    maplist(atom_chars, StringList, Lists),
    append(Lists, List),
    atom_chars(StringResult, List).
