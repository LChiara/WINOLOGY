# Winology

Winology is an intelligent tool that can tell you if you would like or not a wine.

## Installation

Clone the repository or download the .zip 

```
git clone https://github.com/LChiara/WINOLOGY
```

## Usage (CommandLine)

Start swipl and load `classify.pl`
```?- consult('D:/WINOLOGY/src/main/prolog/classify.pl').
?- startLearningProcess. %learn all the rating classes.
?- classify([time=breakfast, food=dessert], X).

X = rating_0
```

## Usage (GUI)

Start swipl and load `winology.pl` to open the GUI
```
?- consult('D:/WINOLOGY/src/main/prolog/winology.pl').
```

## Project structure

    ..
    ├── docs                    # Documentation files
    ├── src                     # Source files
    └── README.md

### Source files

    ..
    ├── ...
    ├── src
    │   └── main      
    │       ├── prolog
    │       │   ├── backup.pl           # create a backup (.txt file) of the DBs
    │       │   ├── backup.plt          # unit tests of backup.pl
    │       │   ├── characteristics.pl  # attributes description
    │       │   ├── classify.pl         # script to for learn and classification
    │       │   ├── classify.plt        # unit tests for classify.pl
    │       │   ├── constants.pl        # useful constants, such as DB file names
    │       │   ├── GUI.pl              # main script for the GUI
    │       │   ├── module_db_rating.pl # module for rating DB
    │       │   ├── module_db_rating.plt # unit tests for module_db_rating.pl
    │       │   ├── module_db_wine.pl   # module for wine DB
    │       │   ├── module_db_wine.plt  # unit tests for module_db_wine.pl
    │       │   ├── register.plt        # script to register a new wine in the DB or a new rating
    │       │   └── winology.pl         # main script to start the GUI
    │       └── resources
    └── ...

## How to describe a Wine?

The description can be splitted into two categories, the situation (when, who, which food) and the wine characteristics.

### Situation

```
rating(<rating>, [time=<timeValue>, food=<foodValue>, person=<personName>, wine=<wineName>]).
```

* `rating`: rating_0; rating_1; rating_2; rating_3; rating_4; rating_5.
* `time`: breakfast; lunch; aperitivo; dinner.
* `food`: meat; fish; vegetarian; pasta; pizza; aperitivo; dessert.
* `person`: ambra; chiara; claudia.
* `wine`: name of the wine.

### Wine

```
wineDescription(<wine>, [aroma=<aromaValue>, body=<bodyValue>, color=<colorValue>, effervescence=<effervescenceValue>, sweetness=<sweetnessValue>]).
```

* `aroma`: aromatic; floreal; fruity; mineral.
* `body`: light_bodied; medium_bodied; full_bodied.
* `color`: yellow; rose; red.
* `effervescence`: yes; no.
* `sweetness`: sweet; dry; off_dry.

## HowTo: Add a new Wine and Rating

The modules module_db_rating and module_db_wine allows the user to populate the DB:

```
% Retrieve all the object in the DB with a specific rating.
?- findall(X, get_rating(rating(rating_0, X)), Objects)

% Register a new entry
?- set_rating(rating(rating_0, [time=breakfast, food=cereals, person=chiara, wine=moscatoDAsti])). 
true.

% Retrieve all the wine facts (already registered in the DB).
?- findall(X, get_wine(wineDescription(X, Y)), Objects).

% Register a new entry in the DB.
?- set_wine(wineDescription(barolo, [aroma=mineral, body=light_bodied, color=red, effervescence=no, sweetness=off_dry])).
true.
```

The function `register(+Rating, +RatingEntry, +WineName, +WineEntry)` adds a new rating and a new wine entry in the respective DBs.
```
register('rating_2', 
         '[time=lunch, food=fish, person=ambra, wine=barolo]', 
         'barolo', 
         '[aroma=mineral, body=light_bodied, color=red, effervescence=no, sweetness=off_dry]'
         ).
```

