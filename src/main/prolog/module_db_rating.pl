/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  module_db_rating.pl

 Description:
 The new custome module "db_rating" uses a persistency library to
 save and restore the state of dynamic predicates between runs of
 Prolog. That means, new facts can be added to the DB,
 any existing fact can be removed, setter and getter are available.

 Persistency library: provide persistent dynamic predicates:
 https://www.swi-prolog.org/pldoc/man?section=persistency

 Usage example:
 %The database "db_rating.pl" will be created if it does not
 exist. ?- attach_rating('db_rating.pl'). true.

 % Retrieve all the rating facts (already registered in the DB) that matches with "rating_0".
 ?- findall(X, get_rating(rating(rating_0, X)), Objects). Objects =
 [[time=breakfast, food=cereals, person=chiara, wine=moscatoDAsti]].

 % Retrieve all the rating facts (already registered in the DB) that matches with "rating_1".
 ?- findall(X, get_rating(rating(rating_1, X)), Objects). Objects =
 [[time=breakfast, food=fish, person=chiara, wine=moscatoDAsti]].

 % Register a new entry in the DB.
 ?- set_rating(rating(rating_0, [time=breakfast, food=cereals,
 person=chiara, wine=moscatoDAsti])). true.

 % Get the already registered entries and the new entry.
 ?- findall(X, get_rating(rating(rating_0, X)), Objects).
 Objects = [[time=breakfast, food=cereals, person=chiara,
 wine=moscatoDAsti],[time=breakfast, food=cereals, person=chiara,
 wine=moscatoDAsti]].
*/

:- module(module_db_rating,
          [ attach_rating_db/1, % +File
            detach_rating_db/0, % +File
            get_rating/1,    % ?Fact
            set_rating/1     % +Fact
          ]).
:- use_module(library(persistency)).

:- persistent
        rating_fact(fact:any).

attach_rating_db(File) :-
        db_attach(File, []).

detach_rating_db :-
        db_detach.

get_rating(rating(rating_0, [time=breakfast, food=cereals,   person=chiara,  wine=moscatoDAsti])).
get_rating(rating(rating_0, [time=breakfast, food=cereals,   person=chiara,  wine=proseccoTerreNardinVenti2])).
get_rating(rating(rating_1, [time=breakfast, food=fish,      person=chiara,  wine=moscatoDAsti])).
get_rating(rating(rating_2, [time=lunch,     food=fish,      person=chiara,  wine=primitivoDiManduria])).
get_rating(rating(rating_3, [time=aperitivo, food=aperitivo, person=chiara,  wine=primitivoDiManduria])).
get_rating(rating(rating_3, [time=aperitivo, food=aperitivo, person=chiara,  wine=vernacciaSerrapetrona])).
get_rating(rating(rating_4, [time=dinner,    food=dinner,    person=chiara,  wine=primitivoDiManduria])).
get_rating(rating(rating_5, [time=lunch,     food=fish,      person=chiara,  wine=luganaSanti])).
get_rating(rating(rating_5, [time=dinner,    food=dessert,   person=chiara,  wine=vernacciaSerrapetrona])).
get_rating(rating(rating_5, [time=lunch,     food=meat,      person=chiara,  wine=fragolinoTerreDelSole])).
get_rating(rating(rating_5, [time=dinner,    food=pizza,     person=chiara,  wine=fragolinoTerreDelSole])).

get_rating(Fact) :-
        rating_fact(Fact).

set_rating(Fact) :-
        findall(X, get_rating(X), Objects),
        \+ member(Fact, Objects),
        assert_rating_fact(Fact).
set_rating(_) :- !.






