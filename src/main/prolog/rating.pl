:-ensure_loaded(wine).

% Rating classes -> rating(<rating>, [_time_, _food_, _person_, _wine_])
% _wine_ is one of the above classes

% Rating 0
rating(rating_0, [time=breakfast, food=cereals, person=chiara,  wine=myWine]).
% rating(rating_0, [time=breakfast, food=cereals, person=chiara,
% wine=otherWine]).

% Rating 1
rating(rating_1, [time=breakfast, food=fish, person=chiara,  wine=otherWine]).

% Rating 2
% rating(rating_2, [time=lunch, food=cereals, person=chiara,
% wine=otherWine]).

% Rating 3
% rating(rating_3, [time=lunch, food=fish, person=chiara, wine=myWine]).

% Rating 4
% rating(rating_4, [time=aperitivo, food=cheese, person=chiara,
% wine=myWine]).

% Rating 5
% rating(rating_5, [time=dinner, food=meat, person=chiara,
% wine=otherWine]).
