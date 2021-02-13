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
registerWineInDB(WineName, WineDescription) :-
    constant(db_wine, File),
    attach_wine_db(File),
    \+ get_wine(wineDescription(WineName, WineDescription)),
    !,
    set_wine(wineDescription(WineName, WineDescription)),
    detach_wine_db.

/* registerRatingInDB(+Rating, +Entry)
 *  Register the wine entry if the wine is not yet in the DB.
 */
registerRatingInDB(Rating, RatingEntry) :-
    constant(db_rating, File),
    attach_rating_db(File),
    set_rating(rating(Rating, RatingEntry)),
    detach_rating_db.





