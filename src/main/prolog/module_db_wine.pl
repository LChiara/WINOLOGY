/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  module_db_wine.pl

 Description:
 The new custome module "db_wine" uses a persistency library to
 save and restore the state of dynamic predicates between runs of
 Prolog. That means, new facts can be added to the DB,
 any existing fact can be removed, setter and getter are available.

 Persistency library: provide persistent dynamic predicates:
 https://www.swi-prolog.org/pldoc/man?section=persistency

 Usage example:
 %The database "db_wine.pl" will be created if it does not exist.
 ?- attach_rating('db_wine.pl').
 true.

 % Retrieve all the wine facts (already registered in the DB).
 ?- findall(X, get_wine(wineDescription(X, Y)), Objects).
 Objects = [fragolinoTerreDelSole, proseccoTerreNardinVenti2,
 moscatoDAsti, verdicchioDiMatelica, vernacciaSerrapetrona,
 primitivoDiManduria].

 %Register a new entry in the DB.
 ?- set_wine(wineDescription(barolo, [aroma=ethereal,
 body=light_bodied, color=red, effervescence=no, sweetness=off_dry])).
 true.

 % Get the already registered entries and the new entry.
 ?-  findall(X, get_wine(wineDescription(X, Y)), Objects).
 Objects = [fragolinoTerreDelSole, proseccoTerreNardinVenti2,
 moscatoDAsti, verdicchioDiMatelica, vernacciaSerrapetrona,
 primitivoDiManduria, barolo].

*/

:- module(module_db_wine,
          [ attach_wine_db/1, % +File
            detach_wine_db/0, % +File
            get_wine/1,    % ?Fact
            set_wine/1     % +Fact
          ]).
:- use_module(library(persistency)).

:- persistent
        wine_fact(fact:any).

attach_wine_db(File) :-
        db_attach(File, []).

detach_wine_db :-
        db_detach.

get_wine(wineDescription(fragolinoTerreDelSole,     [aroma=fruity,   body=light_bodied,  color=red,    effervescence=no,  sweetness=sweet])).
get_wine(wineDescription(proseccoTerreNardinVenti2, [aroma=aromatic, body=light_bodied,  color=yellow, effervescence=yes, sweetness=dry])).
get_wine(wineDescription(moscatoDAsti,              [aroma=aromatic, body=light_bodied,  color=yellow, effervescence=yes, sweetness=off_dry])).
get_wine(wineDescription(verdicchioDiMatelica,      [aroma=fruity,   body=medium_bodied, color=yellow, effervescence=no,  sweetness=off_dry])). % antipasti ok, pesce, formaggi ok -> carne no.
get_wine(wineDescription(vernacciaSerrapetrona,     [aroma=aromatic, body=medium_bodied, color=red,    effervescence=no,  sweetness=sweet])).
get_wine(wineDescription(primitivoDiManduria,       [aroma=aromatic, body=full_bodied,   color=red,    effervescence=no,  sweetness=off_dry])).
get_wine(wineDescription(luganaSanti,               [aroma=floreal,  body=light_bodies,  color=yellow, effervescence=yes, sweetnees=sweet])).

get_wine(Fact) :-
        wine_fact(Fact).

set_wine(Fact) :-
        findall(X, get_wine(X), Objects),
        \+ member(Fact, Objects),
        assert_wine_fact(Fact).
set_wine(_) :- !.
