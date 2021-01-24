:- ensure_loaded(characteristics).

% Wine classes wine(<wineName>, <wineDescription>).
% wineDescription(myWine, [ limpidezza=velato, colore=giallo,
% effervescenza=si, descrizione=minerale, intensita=poco_intenso]).
% wineDescription(otherWine, [ limpidezza=velato, colore=rosso,
% effervescenza=no, descrizione=aromatico, intensita=intenso]).


wineDescription(fragolinoTerreDelSole,     [aroma=fruity,   body=light_bodied, color=red,    effervescence=no,  sweetness=sweet]).
wineDescription(proseccoTerreNardinVenti2, [aroma=aromatic, body=light_bodied, color=yellow, effervescence=yes, sweetness=dry]).
wineDescription(moscatoDAsti,              [aroma=aromatic, body=light_bodied, color=yellow, effervescence=yes, sweetness=off_dry]).
wineDescription(verdicchioDiMatelica,      [aroma=fruity,   body=medium_bodied, color=yellow, effervescence=no, sweetness=off_dry]). % antipasti ok, pesce, formaggi ok -> carne no.
wineDescription(luganaSanti,               [aroma=floreal,  body=light_bodies, color=yellow, effervescence=yes, sweetnees=sweet]).
wineDescription(vernacciaSerrapetrona,     [aroma=aromatic, body=medium_bodied, color=red, effervescence=no, sweetness=sweet]).
wineDescription(primitivoDiManduria,       [aroma=aromatic, body=full_bodied, color=red, effervescence=no, sweetness=off_dry]).

