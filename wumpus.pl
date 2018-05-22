:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% [Done 15 May 2018] Initialize game state
%% ------------------------------------------------------------------
%% Game state is consist of
%%      -> A traversed Map (always from the output of updateState)
%%      -> A list of steps (planned to traverse)
%%      -> informations that is needed thoughout the guess, such as 
%%         map size and energy
%%      -> instructions for shooting
%% format of Map: [Empty,Pit,Wall,Suspicious,Wumpus]. Each is a list of of X-Y
initialState(NR,NC,XS,YS,[[[XS-YS],[],Wall,[],0-0],[XS-YS],[NR,NC,100],[]]):-
    createWall(NR,NC,Wall).


%% [Half Done 16 May 2018] Able to traverse all blocks in the map 
%% routes does not guarantee a non repetition list (possibly traverse 
%% a block for several times)
%% -------------------------------------------------------------------
%% Map [[X,Y,_]]-> Map construction from the last robot
%% Steps [X-Y]-> traversed blocks following the sequence of Guess
guess(State1,State2,Guess):-
    nl,write("Update output: "),write(State1),
    State1 = [Map,_,_,_],
    Map = [_,_,_,_,Wumpus],
    (   Wumpus == 0-0 ->
        %% wumpus not found
        travMap(State1,State2,[],Guess)
    ;   write("try to shoot"),   
    tryShoot(State1,State2,Guess)
        ).


%% [Half Done 16 May 2018] update new map, traversed history
%% [Maybe Done 17 May 2018] Stores blocks in diff arrays
updateState(State0,Guess,Feedback,State1):-
    nl,write("Guess -- State: "), write(State0),nl,write(" | Guesses: "),write(Guess),nl,write(" | Feedback: "),write(Feedback),nl,
    State0 = [OldMap,_,Info,Inst],
    Info = [NR,NC,_],
    NewInfo = [NR,NC,100],
    updateMap(OldMap,Inst,Guess,Feedback,Info,NewMap,NewInst),
    map2List(NewMap,NMlist),
    cop2Step(NMlist,NR,NC,NewSteps),
    NewMap = [_,_,_,_,Wumpus],
    (   Wumpus == 0-0 ->
            State1 = [NewMap,NewSteps,NewInfo,Inst]
        ;
            State1 = [NewMap,NewSteps,NewInfo,NewInst]
        )

    .



%% Sequence
%% -> if not running out of energy or the map is supposed to be traversed
%%      -> Get a new untraversed position
%%      -> Find path to that position
%%      -> Construct a list of steps
%%      -> Call itself
%% -> if map is done or run out of energy
%%      -> Guess and State will be the same as last one
travMap(State1,State2,GuessHist,Guess):-
    State1 = [OldMap,OldSteps,Info,Inst],
    OldSteps = [X1-Y1|_],
    Info = [NR,NC,EN],
    OldMap=[_,_,_,Suspicious,_],
    myL(OldSteps,S1),myL(Suspicious,S2),S is S1 + S2, MS is NR * NC,
    (   EN > 0, S < MS->
            nextDes(NR,NC,OldSteps,OldMap,XP,YP,NewMap),
            write("Next Destination: "),write(XP),write(YP),nl,
            findPath(X1,Y1,XP,YP,EN,ENP,NR,NC,NewMap,Guess1),
            constSteps(X1-Y1,NR,NC,EN,Guess1,OldSteps,NewSteps1),
            write("Next Steps: "),write(NewSteps1),nl,
            append(GuessHist,Guess1,GuessHist2),
            Info2 = [NR,NC,ENP],
            travMap([NewMap,NewSteps1,Info2,Inst],State2,GuessHist2,Guess)
    ;   State2 = State1,
        Guess = GuessHist
        ).


tryShoot(State1,State2,Guess):-
    State1=[_,[X-Y|_],_,_],
    tryShoot(State1,State2,[shoot],X-Y,Guess).


tryShoot([Map,Steps,Info,[]],State2,GuessHist,_,Guess):-
%%    write('[[End of Loop: ]]'),
    State2=[Map,Steps,Info,[]],
    Guess = GuessHist.
tryShoot(State1,State2,GuessHist,X-Y,Guess):-
    State1 = [Map,_,Info,Inst],
    Info = [NR,NC,EN],
    Inst = [(XD-YD,Move)|_],
    findPath(X,Y,XD,YD,EN,END,NR,NC,Map,Guess1),
    (END > 6 ->
        append(GuessHist,Guess1,Guess2),
        append(Guess2,[Move],Guess3),
        append(Guess3,[shoot],Guess),
        State2 = State1
        ).


%% [Done 15 May 2018] X, Y that is not traversed
%% [Improved 16 May 2018] auto-generate until X Y is good
nextDes(NR,NC,Steps,Map,X,Y,Map1):-
    Map=[_,_,_,Suspicious,Wumpus],
    myL(Suspicious,S1),myL(Steps,S2),S is S1 + S2,
    MS is NR * NC,
    (S < MS ->
        random_between(1,NC,X1),
        random_between(1,NR,Y1),
    %% if X1-Y1 has not been traversed in this round
        (   \+ member(X1-Y1,Steps),\+ member(X1-Y1,Suspicious)->

            %% if X1-Y1 is not part of the suspicious queue, 
            %% then X1-Y1 is approachable, suspicious queue does
            %% not need to update
            (   \+unapproachable(X1-Y1,Map) ->
                X is X1,Y is Y1,
                Map1 = Map

            %% if X1-Y1 is part of the suspicious queue, then it is 
            %% not approachable, need to generate a new one
            ;   Map = [Empty,Pit,Wall,Suspicious,Wumpus],
                append(Suspicious,[X1-Y1],Suspicious1),
                Map0 = [Empty,Pit,Wall,Suspicious1,Wumpus],
                nextDes(NR,NC,Steps,Map0,X,Y,Map1)
                )
           %% if X1-Y1 has been travered in this round, it is not usable,
           %% need to genrate a new one.
        ;   nextDes(NR,NC,Steps,Map,X,Y,Map1)
        )
    ) .


%% [Done 15 May 2018] find a path and leftover energy given start and destination
findPath(X,Y,X1,Y1,EN,EN1,NR,NC,Map,Path):-
    findPath(X,Y,X1,Y1,EN,EN1,NR,NC,Map,[X-Y],Path).

findPath(_,_,_,_,0,0,_,_,_,_Hist,[]).
findPath(X,Y,X,Y,EN,EN,_,_,_,_Hist,[]).
findPath(X1,Y1,X2,Y2,EN,EN2,NR,NC,Map,Hist,[NMove|Guess]):-
(    EN > 0 ->
    Map = [_,Pit,Wall,Suspicious,Wumpus],
    move(X1,Y1,XP,YP,EN,ENP,NR,NC,NMove),
    Step = XP-YP,
    \+ member(Step,Hist),
    \+ member(Step,[Wumpus]),
    \+ member(Step,Pit),
    \+ member(Step,Wall),
    findPath(XP,YP,X2,Y2,ENP,EN2,NR,NC,Map,[Step|Hist],Guess)
    ).


%% [Done 15 May 2018] able to construct list of traversed blocks using
%% set of instructions, if the block has been traversed before, it will 
%% not be added to the steps
constSteps(_,_,_,_,[],OldSteps,OldSteps).
constSteps(X-Y,NR,NC,EN,[Move|Guess],OldSteps,NewSteps):-
    move(X,Y,X1,Y1,EN,EN1,NR,NC,Move),
    ( member(X1-Y1,OldSteps) ->
        keep(OldSteps,NewSteps1)
    ;   cont(X1-Y1,OldSteps,NewSteps1)
    ),
    constSteps(X1-Y1,NR,NC,EN1,Guess,NewSteps1,NewSteps).


%% [Done 15 May 2018] moving instructions
move(EN,ENN,shoot):-
    ENN is EN - 5.

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




% 


%% [Done 16 May 2018] construct the new map due to the set of
%% instructions and feedback. output -> NewMap

updateMap(OldMap,Inst,Guess,Feedback,Info,NewMap,NewInst):-
    OldMap = [Empty,_,_,_,_],Empty = [X-Y|_],
    updateMap(X-Y,OldMap,Inst,Guess,Feedback,Info,NewMap,NewInst).


updateMap(_,OldMap,Inst,_,[],_,OldMap,Inst).
updateMap(X-Y,OldMap,Inst,Guess,Feedback,Info,NewMap,NewInst):-
    Guess = [Move|Glist],
    Feedback = [Fb|Fblist],
    Info = [NR,NC,_],
    %% at a miss, delete the location from the instruction
    (   Move == shoot, Fb == miss ->
        delete(Inst,(X-Y,_),NewInst1),
        updateMap(X-Y,OldMap,NewInst1,Glist,Fblist,Info,NewMap,NewInst)
    ;


    %% if not a miss, update the map

    %% get the destination for the feedback and update to map if not in it
    %% if is wall -> the next starting point will be this destination
    %% if feedback is wumpus -> construct shooting instructions (makePair)
    %% else the next starting point is this starting point (robot did not move)
   
        map2List(OldMap,Mlist),
        moveM(X,Y,X1,Y1,NR,NC,Move),
        (   \+ member(X1-Y1,Mlist) ->
                consMap(X1-Y1,Fb,OldMap,NewMap1),

                (   %% if is wall, update map only
                    Fb == wall ->
                    updateMap(X-Y,NewMap1,Inst,Glist,Fblist,Info,NewMap,NewInst)
                ;   %% if is wumpus, update map and shooting instructions
                    Fb == wumpus ->
                    makePair(NewMap1,NR,NC,NewInst1),
                    updateMap(X1-Y1,NewMap1,NewInst1,Glist,Fblist,Info,NewMap,NewInst)
                ;    %% if not wall or wumpus, update map and starting point
                   updateMap(X1-Y1,NewMap1,Inst,Glist,Fblist,Info,NewMap,NewInst)
                    )
            
        ;   %% if the destination is in map then no need to update the map
            %% simply start at the destination and go on constructing the map until the end of the feedback
            updateMap(X1-Y1,OldMap,Inst,Glist,Fblist,Info,NewMap,NewInst)
        )
   
    ).
%% 


consMap(BP,Fb,Map0,Map1):-
    Map0 = [Empty,Pit,Wall,Suspicious,Wumpus],
    (   Fb == pit ->
            append(Pit,[BP],Pit1),
            Map1 = [Empty,Pit1,Wall,Suspicious,Wumpus]
    ;   Fb == wall ->
            append(Wall,[BP],Wall1),
            Map1 = [Empty,Pit,Wall1,Suspicious,Wumpus]
    ;   Fb == wumpus ->
            Wumpus1 = BP,
            Map1 = [Empty,Pit,Wall,Suspicious,Wumpus1]
    ;   % temporarily ignoring smell and stench
            append(Empty,[BP],Empty1),
            Map1 = [Empty1,Pit,Wall,Suspicious,Wumpus]
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
    Map = [Empty,Pit,Wall,Suspicious,Wumpus],
    append(Empty,Pit,List0),
    append(List0,Wall,List1),
    append(List1,[Wumpus],List).

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


shootCol(_,0,[]).
shootCol(X,NR,[X-NR|List]):-
    NR > 0, NR1 is NR - 1,
    shootCol(X,NR1,List).

shootRow(_,0,[]).
shootRow(Y,NC,[NC-Y|List]):-
    NC > 0, NC1 is NC - 1,
    shootRow(Y,NC1,List).

%% [Done 17 May 2018] using map information to constructed
%% a list of instructions [(X-Y, Move)] telling the destination
%% and the next move.
makePair(Map,NR,NC,List):-
    Map = [_,Pit,Wall,Suspicious,Wumpus],
    append(Pit,Wall,L1),
    append(L1,Suspicious,L2),
    append([Wumpus],L2,Flist),
    Wumpus = X-Y,
    shootCol(X,NR,ListC), shootRow(Y,NC,ListR),
    makePair(ListC,Flist,NR,NC,[],List1),
    makePair(ListR,Flist,NR,NC,[],List2),
    append(List1,List2,List).

makePair([],_,_,_,Last,Last).
makePair([Pos],_,_,_,Last,Last).
makePair(List,Flist,NR,NC,List0,List1):-
    Flist = [XW-YW|_],
    List = [Pos|Plist],Plist = [NextPos|_],
    (   \+ member(Pos,Flist), \+ member(NextPos,Flist) ->
            Pos = X1-Y1, NextPos = X2-Y2,
            Diff1 is abs(X1+Y1-YW-XW),
            Diff2 is abs(X2+Y2-YW-XW),
            (   Diff1 > Diff2 ->
                    moveM(X1,Y1,X2,Y2,NR,NC,Move),
                    NewPair = (X1-Y1,Move)
            ;       moveM(X2,Y2,X1,Y1,NR,NC,Move),
                    NewPair = (X2-Y2,Move)
                ),
            append([NewPair],List0,List2),
            makePair(Plist,Flist,NR,NC,List2,List1)
        ;
            makePair(Plist,Flist,NR,NC,List0,List1)
        ).

createWall(NR,NC,Wall):-
    NR1 is NR + 1, NC1 is NC + 1,
    createRow(0,NC1,Wall1),
    createRow(NR1,NC1,Wall2),
    createCol(NR,0,Wall3),
    createCol(NR,NC1,Wall4),

    append(Wall1,Wall2,Wall12),
    append(Wall3,Wall4,Wall34),
    append(Wall12,Wall34,Wall).

createRow(R,0,[R-0]).
createRow(R,C,[R-C|Walllist]):-
( C > 0 ->
    C1 is C - 1,
    createRow(R,C1,Walllist)
    ).

createCol(0,C,[]).
createCol(R,C,[R-C|Walllist]):-
(   R > 0 ->
    R1 is R - 1,
    createCol(R1,C,Walllist)
    ).


%% check if X-Y is surrounded by walls and pits
unapproachable(X-Y,Map):-
    X1 is X - 1, X2 is X + 1,
    Y1 is Y - 1, Y2 is Y + 1,
    Map=[_,Pit,Wall,Suspicious,_],
    append(Pit,Wall,List),
    append(List,Suspicious,NotApprocable),
    member(X-Y2,NotApprocable),
    member(X-Y1,NotApprocable),
    member(X1-Y,NotApprocable),
    member(X2-Y,NotApprocable).

