/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  backup.plt

 Unit tests for backup
*/

readFile(File, Lines) :-
    open(File, read, Str),
    read_file(Str, Lines),
    close(Str),
    write(Lines), nl.

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Tmp),
    Tmp \= end_of_file,
    X = Tmp,
    read_file(Stream, L).

read_file(_, [_|_]) :- !.

sublist([], _).
sublist([H|T], [H|TT] ) :- sublist( T, TT ).
sublist([H|T], [_|TT] ) :- sublist( [H|T], TT ).

:- begin_tests(backup).

:- use_module(backup).
:- use_module(module_db_rating).
:- use_module(module_db_wine).

test(create_backup) :-
    createBackup,
    readFile('wine.backup', WineContent),
    readFile('rating.backup', RatingContent),
    findall(X, get_rating(rating(rating_0, X)), R0),
    sublist(R0, RatingContent),
    findall(X, get_rating(rating(rating_1, X)), R1),
    sublist(R1, RatingContent),
    findall(X, get_rating(rating(rating_2, X)), R2),
    sublist(R2, RatingContent),
    findall(X, get_rating(rating(rating_3, X)), R3),
    sublist(R3, RatingContent),
    findall(X, get_rating(rating(rating_4, X)), R4),
    sublist(R4, RatingContent),
    findall(X, get_rating(rating(rating_5, X)), R5),
    sublist(R5, RatingContent),
    findall(X, get_wine(X), Wine),
    sublist(Wine, WineContent).

:- end_tests(backup).
