:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% [Done 15 May 2018] Initialize game state
%% ------------------------------------------------------------------
%% Game state is consist of
%%      -> A traversed Map (always from the output of updateState)
%%      -> A list of steps (planned to traverse)
%%      -> informations that is needed thoughout the guess, such as 
%%         map size and energy
%% format of Map: [Empty,Pit,Wall,Wumpus]. Each is a list of of X-Y
initialState(NR,NC,XS,YS,[[[XS-YS],[],[],0-0],[XS-YS],[NR,NC,100]]).


%% ===================================================================
%% *******************************************************************


%% [Half Done 16 May 2018] Able to traverse all blocks in the map 
%% routes does not guarantee a non repetition list (possibly traverse 
%% a block for several times)
%% -------------------------------------------------------------------
%% Map [[X,Y,_]]-> Map construction from the last robot
%% Steps [X-Y]-> traversed blocks following the sequence of Guess
guess(State1,State2,Guess):-
    State1 = [Map,_,_],
    Map = [_,_,_,Wumpus],
    (   Wumpus == 0-0 ->
        %% wumpus not found
        travMap(State1,State2,[],Guess)
    ;   write(1)
        ).
%%    travMap(State1,State2,[],Guess).

%% Sequence
%% -> if not running out of energy or the map is supposed to be traversed
%%      -> Get a new untraversed position
%%      -> Find path to that position
%%      -> Construct a list of steps
%%      -> Call itself
%% -> if map is done or run out of energy
%%      -> Guess and State will be the same as last one


travMap(State1,State2,GuessHist,Guess):-
    State1 = [OldMap,OldSteps,Info],
    OldSteps = [X1-Y1|_],
    Info = [NR,NC,EN],
    myL(OldSteps,S), MS is NR * NC,
    (   EN > 0, S < MS->
            nextDes(NR,NC,OldSteps,XP,YP),
            write(XP),write(YP),nl,
            findPath(X1,Y1,XP,YP,EN,ENP,NR,NC,OldMap,Guess1),
            constSteps(X1-Y1,NR,NC,EN,Guess1,OldSteps,NewSteps1),
            append(GuessHist,Guess1,GuessHist2),
            Info2 = [NR,NC,ENP],
            travMap([OldMap,NewSteps1,Info2],State2,GuessHist2,Guess)
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
findPath(X,Y,X1,Y1,EN,EN1,NR,NC,Map,Path):-
    findPath(X,Y,X1,Y1,EN,EN1,NR,NC,Map,[X-Y],Path).

findPath(_,_,_,_,0,0,_,_,_,_Hist,[]).
findPath(X,Y,X,Y,EN,EN,_,_,_,_Hist,[]).
findPath(X1,Y1,X2,Y2,EN,EN2,NR,NC,Map,Hist,[NMove|Guess]):-
    EN > 0,Map = [_,Pit,Wall,_],
    move(X1,Y1,XP,YP,EN,ENP,NR,NC,NMove),
    Step = XP-YP,
    \+ member(Step,Hist),
    \+ member(Step,Wall),
    \+ member(Step,Pit),
    findPath(XP,YP,X2,Y2,ENP,EN2,NR,NC,Map,[Step|Hist],Guess).


%% [Done 15 May 2018] able to construct list of traversed blocks using
%% set of instructions
constSteps(_,_,_,_,[],OldSteps,OldSteps).
constSteps(X-Y,NR,NC,EN,[Move|Guess],OldSteps,NewSteps):-
    move(X,Y,X1,Y1,EN,EN1,NR,NC,Move),
    ( member(X1-Y1,OldSteps) ->
        keep(OldSteps,NewSteps1)
    ;   cont(X1-Y1,OldSteps,NewSteps1)
    ),
    constSteps(X1-Y1,NR,NC,EN1,Guess,NewSteps1,NewSteps).


%% [Done 15 May 2018] moving instructions
move(X,Y,XN,YN,EN,ENN,_,_,north):-
    (   Y > 1 ->
            YN is Y - 1,
            XN is X,
            ENN is EN - 1
    ).

move(X,Y,XN,YN,EN,ENN,NR,_,south):-
    (   Y < NR ->
            YN is Y + 1,
            XN is X,
            ENN is EN - 1
    ).

move(X,Y,XN,YN,EN,ENN,_,NC,east):-
    (   X < NC ->
            XN is X + 1,
            YN is Y,
            ENN is EN - 1
    ).

move(X,Y,XN,YN,EN,ENN,_,_,west):-
    (   X > 1 ->
            XN is X - 1,
            YN is Y,
            ENN is EN - 1
    ).



cont(X-Y,Step,[X-Y|Step]).
keep(Step,Step).

myL([],0).
myL([_|List],N):-
    myL(List,N1),
    N is N1 + 1.



%% ========================================================================
%% ************************************************************************


%% [Half Done 16 May 2018] update new map, traversed history
%% [Maybe Done 17 May 2018] Stores blocks in diff arrays
updateState(State0,Guess,Feedback,[NewMap,NewSteps,NewInfo]):-
    State0 = [OldMap,_,Info],
    Info = [NR,NC,_],
    NewInfo = [NR,NC,100],
    updateMap(OldMap,Guess,Feedback,Info,NewMap),
    map2List(NewMap,NMlist),
    cop2Step(NMlist,NR,NC,NewSteps).

% 


%% [Done 16 May 2018] construct the new map due to the set of
%% instructions and feedback. output -> NewMap

updateMap(OldMap,Guess,Feedback,Info,NewMap):-
    OldMap = [Empty,_,_,_],Empty = [X-Y|_],
    updateMap(X-Y,OldMap,Guess,Feedback,Info,NewMap).


updateMap(_,OldMap,_,[],_,OldMap).
updateMap(X-Y,OldMap,Guess,Feedback,Info,NewMap):-
    Guess = [Move|Glist],
    Feedback = [Fb|Fblist],
    Info = [NR,NC,_],
    map2List(OldMap,Mlist),
    moveM(X,Y,X1,Y1,NR,NC,Move),
    (   \+ member(X1-Y1,Mlist) ->
            consMap(X1-Y1,Fb,OldMap,NewMap1),
            updateMap(X1-Y1,NewMap1,Glist,Fblist,Info,NewMap)
    ;       updateMap(X1-Y1,OldMap,Glist,Fblist,Info,NewMap)
        ).
%% 


consMap(BP,Fb,Map0,Map1):-
    Map0 = [Empty,Pit,Wall,Wumpus],
    (   Fb == pit ->
            cont(BP,Pit,Pit1),
            Map1 = [Empty,Pit1,Wall,Wumpus]
    ;   Fb == wall ->
            cont(BP,Wall,Wall1),
            Map1 = [Empty,Pit,Wall1,Wumpus]
    ;   Fb == wumpus ->
            Wumpus1 = BP,
            Map1 = [Empty,Pit,Wall,Wumpus1]
    ;   % temporarily ignoring smell and stench
            cont(BP,Empty,Empty1),
            Map1 = [Empty1,Pit,Wall,Wumpus]
        ).
    

%% Steps shows how much the map has been constructed , but a map includes 
%% edges, the steps don't
cop2Step([],_,_,[]).
cop2Step(Map,NR,NC,Steps):-
    Map = [X-Y|MapList],
(   inBound(X,Y,NR,NC) ->
    cont(X-Y,Steps1,Steps),

    cop2Step(MapList,NR,NC,Steps1)
;   cop2Step(MapList,NR,NC,Steps)
    ).

map2List(Map,List):-
    Map = [Empty,Pit,Wall,_],
    append(Empty,Pit,List0),
    append(List0,Wall,List).

%% [Done 16 May 2018] get status of next block using current position,
%% movement and feedback
updateBlock(X1-Y1,NR,NC,Move,Fb,X2-Y2,X3-Y3):-
    moveM(X1,Y1,X2,Y2,NR,NC,Move),
    (   Fb == wall ->
            X3 is X1,Y3 is Y1
    ;       move(X1,Y1,X3,Y3,10,_,NR,NC,Move)
        ).

newPos(X1-Y1,NR,NC,Move,Fb,X2-Y2):-
    (   Fb == wall ->
            X2 is X1,Y2 is Y1
    ;   move(X1,Y1,X2,Y2,10,_,NR,NC,Move)
        ).

moveM(X,Y,XN,YN,_,_,north):-
    Y >= 1 ,
    YN is Y - 1,
    XN is X.

moveM(X,Y,XN,YN,NR,_,south):-
    Y =< NR,
    YN is Y + 1,
    XN is X.

moveM(X,Y,XN,YN,_,NC,east):-
    X =< NC,
    XN is X + 1,
    YN is Y.

moveM(X,Y,XN,YN,_,_,west):-
    X >= 1,
    XN is X - 1,
    YN is Y.

inBound(X,Y,R,C):-
    X > 0,
    X =< C,
    Y > 0,
    Y =< R.

%% find a place that in sequence and not in pit or wall
%% come up with a list of pairs (X-Y, Move)(as sometimes
%% we need to move towards the wall)
shotPos(Map,Pairs):-
    Map = [_,Pit,Wall,XW-YW],
    XRL is XW - 1, XRH = XW + 1,
    YRL is YW - 1, YRH = YW + 1,
    makePair().

makePair(NR,NC,Wumpus,Pair):-
    Wumpus = X-Y.

myZ(_,0,[]).
myZ(X,NR,[X-NR|List]):-
    NR is NR1 + 1.
    myZ(X,NR1,List).

