:- ensure_loaded(characteristics).
:- ensure_loaded(wine_backup).
:- ensure_loaded(rating_backup).

:- op(300,xfx,<==).

/* classify( +Description, -RatingClass)
 *  Oggetto: [Attr1=Val1, .. ,AttrN=ValN]
 *  RatingClass: rating class, that could be described using
 *  Attr=Val.
 * Premise: learning is already done. */

classify(SituationDescription, RatingClass) :-
	RatingClass <== Description,
	member(CongiunzioneAttributi, Description),
	satisfy(SituationDescription, CongiunzioneAttributi).

/* learn( +Class ):  gathers the training set in a list, to induce rules for the class description.
 *  Classe:  Class for which all the rules shall be induced /learnt.
 */
learn(RatingClass) :-
	bagof( rating(C,O), rating(C,O), SimulationDescriptions ),
	resolveDescriptions( SimulationDescriptions, VerboseDescriptions ),
	learn( VerboseDescriptions, RatingClass, Description ),
	nl,write( RatingClass ),write('<=='),nl,
	writelist( Description ),
	assert( RatingClass <== Description ).

% learn(Examples, RatingClass, Description)
learn( Examples, RatingClass, [] ) :-
	\+ member( rating(RatingClass,_), Examples ).
learn( Examples, RatingClass, [ Rule | Rules ] ) :-
	induceRule( Examples, RatingClass, Rule ),
	remove( Examples, Rule, FurtherExamples ),
	learn( FurtherExamples, RatingClass, Rules ).

% resolveDescriptions( +Descriptions, VerboseDescriptions )
% resolve the wine in the descriptions using all the wineAttributes
resolveDescriptions( [], _) :- !.
resolveDescriptions( [rating(RatingClass, Description) | Descriptions], VerboseDescriptions ):-
	member(wine=WineVal, Description),
	wineObj(WineVal, WineDescription),
	append(Description, WineDescription, VerboseDescription),
	append(NewVerboseDescriptions, [rating(RatingClass, VerboseDescription)], VerboseDescriptions),
	resolveDescriptions(Descriptions, NewVerboseDescriptions).

% satisfy(+SituationDescription, +Rule)
% Check if the Attribute has the same value in the SituationDescription and in the Rule.
satisfy( SituationDescription, Condition) :-
	\+ (member(Att=Valx, Condition), member(Att=Valy, SituationDescription), Valx \== Valy).


% induce_rule(+SituationDescriptions, +RatingClass, Rule)
% Conditions in a list of Attr=Val, satisfied by some examples (SituationDescriptions) and by no negative examples.
induceRule( SituationDescriptions, RatingClass, []) :-
    \+ ( member( rating(OtherRating,_), SituationDescriptions),
    OtherRating \== RatingClass),
    !.
induceRule( SituationDescriptions, OtherRating, [ AttrVal | Conds ] ) :-
	chooseAttrVal( SituationDescriptions, OtherRating, AttrVal ),
	filter( SituationDescriptions, [AttrVal], FilteredSituations ),
	induceRule( FilteredSituations, OtherRating, Conds ).

% chooseAttrVal(+SituationDescriptions, +RatingClass, +AttVal)
% Get a score for each AttVal in the SituationDescriptions getting the one with the highest score.
chooseAttrVal( SituationDescriptions, RatingClass, AttVal) :-
	findall( AV/Score,
        scoreAttVal( SituationDescriptions, RatingClass, AV, Score ),
        AVs ), % Score = promettenza di essere AttrVal davvero discriminante.
	best( AVs, AttVal).

% best(+AttValList, AttVal)
% Get the AttVal with the highest score.
best([AttVal/_],AttVal).
best([AV0/S0,AV1/S1|AVSlist],AttVal) :-
	S1 > S0, !,    % AV1 è meglio di AV0
	best([AV1/S1|AVSlist],AttVal)
	;
	best([AV0/S0|AVSlist],AttVal).

% scoreAttVal(+SituationDescriptions, +RatingClass, +AttVal, Score)
% calculate the score of the attribute, basing on how many positive examples matches (satisfy) Attr=Val.
scoreAttVal( SituationDescriptions, RatingClass, AttVal, Score ) :-
	candidate( SituationDescriptions, RatingClass, AttVal),  % get a suitable AttVal couple
	filter( SituationDescriptions, [AttVal], FilteredDescriptions ),  % gli Examples1 soddisfano la condizione Att=Val
	length( FilteredDescriptions, N1 ),
	countPositive( FilteredDescriptions, RatingClass, PositiveCount ),  % numero di Examples positivi
	PositiveCount > 0,                        % almeno un Exampleso positivo
	Score is (PositiveCount + 1) / (N1 + 2).

% candidate(+AttVal, +SituationDescriptions, +RatingClass)
% search a rule candidate in the list of attributes.
candidate( SituationDescriptions, RatingClass, Att=Val ) :-
	characteristic( Att, Values ),
	member( Val, Values ),
	suitable( Att=Val, SituationDescriptions, RatingClass ). % at least, one situationDescription (negative example) shall not have Att=Val

% suitable(+AttVal, +SituationDescriptions, +RatingClass)
% check if the Att=Val is suitable as rule: at least one of the negative example shall not match Att=Val.
suitable( AttVal, SituationDescriptions, RatingClass ) :-
	member( rating(RatingClassX, SituationDescriptionX), SituationDescriptions ),
	RatingClassX \== RatingClass,		% negative (ActualClass is not the TargetClass [= class to learn])
	\+ satisfy( SituationDescriptionX, [AttVal] ), !.

% filter(+SituationDescriptions, +Condition, FilteredExamples)
% Filter the SituationDescriptions basing on the Condition: all the filtered examples contain the condition.
filter(SituationDescriptions, Condition, FilteredExamples) :-
	findall(
        rating(RatingClass, SituationDescription),
        (member( rating(RatingClass, SituationDescription), SituationDescriptions), satisfy(SituationDescription, Condition)),
        FilteredExamples).

% countPositive(+SituationDescriptions, +RatingClass, N)
% Count number of positive examples
countPositive([],_,0).
countPositive([ rating(RatingClassX,_) | SituationDescriptions ], RatingClass, N) :-
	countPositive(SituationDescriptions, RatingClass, N1),
	(RatingClassX=RatingClass, !, N is N1+1 ; N=N1).

% remove(+SituationDescriptions, +Rule, FilteredExamples)
% Remove from SituationDescriptions all the examples covered by Rule, setting FilteredExamples
remove([],_,[]).
remove([ rating(_, SituationDescription) | Situations ], Rule, FilteredSituations) :-
	satisfy(SituationDescription, Rule), !, % remove the first example that matches rule
	remove(Situations, Rule, FilteredSituations).
remove([ SituationDescription | SituationDescriptions ], Rule, [ SituationDescription | FilteredSituations ]) :-
	remove(SituationDescriptions, Rule, FilteredSituations).


writelist([]).
writelist([X|L]) :-
	tab(2), write(X), nl,
	writelist(L).



