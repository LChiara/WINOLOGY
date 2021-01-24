/*

 * Author: Chiara Leonori
 * E-Mail: chiaraleonori@gmail.com
 * Title:  module_db_wine.plt

 Unit tests for module_db_wine
*/

:- begin_tests(module_db_wine).
:- use_module(module_db_wine).

test(attachDB, setup(detach_wine_db)) :-
    attach_wine_db('tmpW.pl').

test(getWine, setup(detach_wine_db)) :-
    attach_wine_db('tmpW.pl'),
    findall(X, get_wine(X), Objects),
    assertion(Objects == [
                  wineDescription(fragolinoTerreDelSole,     [aroma=fruity,   body=light_bodied,  color=red,    effervescence=no,  sweetness=sweet]),
                  wineDescription(proseccoTerreNardinVenti2, [aroma=aromatic, body=light_bodied,  color=yellow, effervescence=yes, sweetness=dry]),
                  wineDescription(moscatoDAsti,              [aroma=aromatic, body=light_bodied,  color=yellow, effervescence=yes, sweetness=off_dry]),
                  wineDescription(verdicchioDiMatelica,      [aroma=fruity,   body=medium_bodied, color=yellow, effervescence=no,  sweetness=off_dry]),
                  wineDescription(vernacciaSerrapetrona,     [aroma=aromatic, body=medium_bodied, color=red,    effervescence=no,  sweetness=sweet]),
                  wineDescription(primitivoDiManduria,       [aroma=aromatic, body=full_bodied,   color=red,    effervescence=no,  sweetness=off_dry]),
                  wineDescription(luganaSanti,               [aroma=floreal,  body=light_bodies,  color=yellow, effervescence=yes, sweetnees=sweet])
              ]).

test(getWine_Fail,  [fail, setup(detach_wine_db)]) :-
    attach_wine_db('tmpW.pl'),
    get_wine(wineDescription(_, [aroma=aromaTest,   body=bodyTest,  color=colorTest,    effervescence=effervescenceTest,  sweetness=sweetnessTest])).

test(setWine, [setup(detach_wine_db),
                 cleanup(detach_wine_db),
                 cleanup(delete_file('./tmpW_1.pl')) ]) :-
    attach_wine_db('tmpW_1.pl'),
    set_wine(wineDescription(test, [aroma=aromaTest,   body=bodyTest,  color=colorTest,    effervescence=effervescenceTest,  sweetness=sweetnessTest])),
    get_wine(wineDescription(X, [aroma=aromaTest,   body=bodyTest,  color=colorTest,    effervescence=effervescenceTest,  sweetness=sweetnessTest])),
    assertion(X == test).

:- end_tests(module_db_wine).

