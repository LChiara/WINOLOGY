/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  constants.pl

 Description:
 Define some useful constants.

*/

constant(db_rating, DB_Rating):-
    getenv('HOME', Value),
    atom_concat(Value, '/winology_rating_db.pl', DB_Rating).

constant(db_wine, DB_Wine) :-
    getenv('HOME', Value),
    atom_concat(Value, '/winology_wine_db.pl', DB_Wine).

% constant(db_wine, 'wine_db.pl').
% constant(db_rating, 'rating_db.pl').

constant(empty_value, '--').
