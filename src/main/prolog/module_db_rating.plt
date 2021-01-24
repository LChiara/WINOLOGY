/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  module_db_rating.plt

 Unit tests for module_db_rating
*/

:- begin_tests(module_db_rating).
:- use_module(module_db_rating).

test(attachDB, setup(detach_rating_db)) :-
    attach_rating_db('tmp.pl').

test(getRating0) :-
    findall(X, get_rating(rating(rating_0, X)), Objects),
    assertion(Objects == [
                  [time=breakfast, food=cereals, person=chiara, wine=moscatoDAsti],
                  [time=breakfast, food=cereals, person=chiara, wine=proseccoTerreNardinVenti2]
              ]).
test(getRating1) :-
    findall(X, get_rating(rating(rating_1, X)), Objects),
    assertion(Objects == [
                  [time=breakfast, food=fish, person=chiara, wine=moscatoDAsti]
              ]).
test(getRating2) :-
    findall(X, get_rating(rating(rating_2, X)), Objects),
    assertion(Objects == [
                  [time=lunch, food=fish, person=chiara, wine=primitivoDiManduria]
              ]).
test(getRating3) :-
    findall(X, get_rating(rating(rating_3, X)), Objects),
    assertion(Objects == [
                  [time=aperitivo, food=aperitivo, person=chiara, wine=primitivoDiManduria],
                  [time=aperitivo, food=aperitivo, person=chiara, wine=vernacciaSerrapetrona]
              ]).
test(getRating4) :-
    findall(X, get_rating(rating(rating_4, X)), Objects),
    assertion(Objects == [
                  [time=dinner, food=dinner, person=chiara, wine=primitivoDiManduria]
              ]).

test(getRating5) :-
    findall(X, get_rating(rating(rating_5, X)), Objects),
    assertion(Objects == [
                  [time=lunch, food=fish, person=chiara, wine=luganaSanti],
                  [time=dinner, food=dessert, person=chiara, wine=vernacciaSerrapetrona],
                  [time=lunch, food=meat, person=chiara, wine=fragolinoTerreDelSole],
                  [time=dinner, food=pizza, person=chiara, wine=fragolinoTerreDelSole]
              ]).

test(setRating_fail, [fail]) :-
    % no matching entry in DB -> FAIL.
    get_rating(rating(_, [time=timeValue, food=foodValue, person=personValue, wine=wineValue])).

test(setRating, [ setup(detach_rating_db),
                 cleanup(detach_rating_db),
                 cleanup(delete_file('./tmp_1.pl')) ]) :-
    attach_rating_db('tmp_1.pl'),
    set_rating(rating(rating_3,
                      [time=timeValue, food=foodValue, person=personValue, wine=wineValue]
                     )),
    get_rating(rating(X, [time=timeValue, food=foodValue, person=personValue, wine=wineValue])),
    assertion(X == rating_3).

:- end_tests(module_db_rating).
