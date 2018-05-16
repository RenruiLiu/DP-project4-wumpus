:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% [Done 15 May 2018] Initialize game state
%% ------------------------------------------------------------------
%% Game state is consist of
%%      -> A traversed Map (always from the output of updateState)
%%      -> A list of steps (planned to traverse)
%%      -> informations that is needed thoughout the guess, such as 
%%         map size and energy
initialState(NR,NC,XS,YS,[[XS-YS-empty],[XS-YS],[NR,NC,100]]).


%% ===================================================================
%% *******************************************************************


%% [Half Done 16 May 2018] Able to traverse all blocks in the map 
%% routes does not guarantee a non repetition list (possibly traverse 
%% a block for several times)
%% -------------------------------------------------------------------
%% Map [[X,Y,_]]-> Map construction from the last robot
%% Steps [X-Y]-> traversed blocks following the sequence of Guess
guess(State1,State2,Guess):-
    guess(State1,State2,[],Guess).

%% Sequence
%% -> if not running out of energy or the map is supposed to be traversed
%%      -> Get a new untraversed position
%%      -> Find path to that position
%%      -> Construct a list of steps
%%      -> Call itself
%% -> if map is done or run out of energy
%%      -> Guess and State will be the same as last one
guess(State1,State2,GuessHist,Guess):-
    State1 = [OldMap,OldSteps,Info],
    OldSteps = [X1-Y1|_],
    Info = [NR,NC,EN],
    myL(OldSteps,S), MS is NR * NC,
    (   EN > 0, S < MS->
            nextDes(NR,NC,OldSteps,XP,YP),
            write(XP),write(YP),nl,
            findPath(X1,Y1,XP,YP,EN,ENP,NR,NC,Guess1),
            constSteps(X1-Y1,NR,NC,EN,Guess1,OldSteps,NewSteps1),
            append(GuessHist,Guess1,GuessHist2),
            Info2 = [NR,NC,ENP],
            guess([OldMap,NewSteps1,Info2],State2,GuessHist2,Guess)
    ;   State2 = State1,
        Guess = GuessHist
        ).


%% [Done 15 May 2018] X, Y that is not traversed
%% [Improved 16 May 2018] auto-generate until X Y is good
nextDes(NR,NC,Steps,X,Y):-
    random_between(1,NC,X1),
    random_between(1,NR,Y1),
    (   \+ member(X1-Y1,Steps)->
            X is X1,Y is Y1
    ;   nextDes(NR,NC,Steps,X,Y)
    ) .


%% [Done 15 May 2018] find a path and leftover energy given start and destination
findPath(X,Y,X1,Y1,EN,EN1,NR,NC,Path):-
    findPath(X,Y,X1,Y1,EN,EN1,NR,NC,[X-Y],Path).

findPath(_,_,_,_,0,0,_,_,_Hist,[]).
findPath(X,Y,X,Y,EN,EN,_,_,_Hist,[]).
findPath(X1,Y1,X2,Y2,EN,EN2,NR,NC,Hist,[NMove|Guess]):-
(   EN > 0 ->
        move(X1,Y1,XP,YP,EN,ENP,NR,NC,NMove),
        Step = XP-YP,
        \+ member(Step,Hist),
        findPath(XP,YP,X2,Y2,ENP,EN2,NR,NC,[Step|Hist],Guess)
    ).


%% [Done 15 May 2018] able to construct list of traversed blocks using
%% set of instructions
constSteps(_,_,_,_,[],OldSteps,OldSteps).
constSteps(X-Y,NR,NC,EN,[Move|Guess],OldSteps,NewSteps):-
    move(X,Y,X1,Y1,EN,EN1,NR,NC,Move),
    ( member(X1-Y1,OldSteps) ->
        keep(OldSteps,NewSteps1)
    ;   cont(X1,Y1,OldSteps,NewSteps1)
    ),
    constSteps(X1-Y1,NR,NC,EN1,Guess,NewSteps1,NewSteps).


%% [Done 15 May 2018] moving instructions
move(X,Y,XN,YN,EN,ENN,_,_,north):-
    (   Y >= 1 ->
            YN is Y - 1,
            XN is X,
            ENN is EN - 1
    ).

move(X,Y,XN,YN,EN,ENN,NR,_,south):-
    (   Y =< NR ->
            YN is Y + 1,
            XN is X,
            ENN is EN - 1
    ).

move(X,Y,XN,YN,EN,ENN,_,NC,east):-
    (   X =< NC ->
            XN is X + 1,
            YN is Y,
            ENN is EN - 1
    ).

move(X,Y,XN,YN,EN,ENN,_,_,west):-
    (   X >= 1 ->
            XN is X - 1,
            YN is Y,
            ENN is EN - 1
    ).



cont(X,Y,Step,[X-Y|Step]).
keep(Step,Step).

myL([],0).
myL([_|List],N):-
    myL(List,N1),
    N is N1 + 1.



%% ========================================================================
%% ************************************************************************


updateState(State0,Guess,Feedback,State1):-
    State0 = [OldMap,OldSteps,Info],
    Guess = [Move|GList],
    Feedback = [Fb|FbList],
    OldMap = [[XS,YS,Content]|MList].


%% [Done 16 May 2018] construct the new map due to the set of
%% instructions and feedback. output -> NewMap
updateMap(_,Map1,_,[],_,Map1).
updateMap(Pos,Map1,Guess,Feedback,Info,NewMap):-
    Guess = [Move|GList],
    Feedback = [Fb|FbList],
    Info = [NR,NC,_],
    updateBlock(Pos,NR,NC,Move,Fb,XN-YN-Fb,NewPos),

    (   \+ member(XN-YN-_,Map1) ->
            append([XN-YN-Fb],Map1,NewMap1)
        ;   NewMap1 = Map1
        ),

    updateMap(NewPos,NewMap1,GList,FbList,Info,NewMap).

%% updateMap(1-1,[1-1-empty],[south,east],[wall,pit],[5,5,10],NewMap).

updateBlock(X1-Y1,NR,NC,Move,Fb,X2-Y2-Fb,X3-Y3):-
    move(X1,Y1,X2,Y2,10,_,NR,NC,Move),
    (   Fb == wall ->
            X3 is X1,Y3 is Y1
    ;       X3 is X2,Y3 is Y2
        ).

newPos(X1-Y1,NR,NC,Move,Fb,X2-Y2):-
    (   Fb == wall ->
            X2 is X1,Y2 is Y1
    ;   move(X1,Y1,X2,Y2,10,_,NR,NC,Move)
        ).