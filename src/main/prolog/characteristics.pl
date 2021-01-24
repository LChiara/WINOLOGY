% A Situation is described by time, food, person and wine
% characteristics.
characteristic(person,       [chiara, ambra, andrea, claudia]).
characteristic(time,         [breakfast, lunch, aperitivo, dinner]).
characteristic(food,         [meat, fish, vegetables, pasta, pizza, cereals, aperitivo, dessert]).

characteristic(rating, [rating_0, rating_1, rating_2, rating_3, rating_4, rating_5]).
% Technical description of wine using sensories.
% Visual Analysis
%characteristic(clarity,   [velato, limpido, brillante]).
%characteristic(color,       [giallo, rosa, rosso]).
%characteristic(effervescence,[si, no]).
% Olfactory Analysis
% characteristic(description, [aromatico, fruttato, fragrante, erbaceo,
% speziato]). Gustatory Analysis characteristic(intensity, [poco_intenso,
% abbastanza_intenso, molto_intenso]).

characteristic(aroma,         [aromatic, floreal, fruity, ethereal, mineral]). % Wine's "aroma"/"nose" is the smell of the wine in the glass: floareal, citrus, fruity, vegetal, earthy...
characteristic(body,          [light_bodied, medium_bodied, full_bodied]).  % Impression of the weigth ans size of the wine in the mouth (Intensity).
characteristic(color,         [yellow, rose, red]).
characteristic(effervescence, [yes, no]).
characteristic(sweetness,     [dry, off_dry, sweet]).
