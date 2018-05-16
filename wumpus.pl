% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO:  %记录pit和wall
        %记录探索过的路 -> 选探索地点时避免它们
        %可以加强find，让避免走过wumpus和pit和wall

:- module(wumpus,[initialState/5, guess/3, updateState/4]).

initialState(NR, NC, XS, YS, State0):-
    %calculate all coordinates
    getCords(NR,NC,Cords,[]),
    %build Facts
    buildMap(Cords,NR,NC),
    Visited = [(XS,YS)],
    WumpusPosition = unknown,
    Info = [(NR,NC),(XS,YS),WumpusPosition],
    ShootPositions = [],
    Dontgo = [(XS,YS)],
    State0 = (Visited,Info,ShootPositions,Dontgo).


guess(State0, State, Guess):-
    %find path 然后去走
    State0 = (Visited,Info,ShootPositions,Dontgo),
    Info = [Border,StartPoint,WumpusPosition], %Info = [边界/wumpus, 起点]
    (   \+ WumpusPosition == unknown ->
        ShootPositions = [ShootPosition|_],

        %拿所有射击路线，然后选出（一条）可以击杀的路线
        getShootPath(StartPoint,ShootPosition,WumpusPosition,Dontgo,Path),

        append(Path,[shoot],Guess),
        State = State0;
        
        %不知道wumpus位置，继续探索 ！没去的地方

        Border = (NR,NC),
        getCords(NR,NC,Cords,[]),
        subtract(Cords,Visited,[UnVisited|_]),

        find(StartPoint,UnVisited,Guess), %一开始就去地图边界 %找没去过的地方
        State = State0 
    ).
        

updateState(State0, Guess, Feedback, State):-
    write(Guess),nl,
    write(Feedback),nl,

    State0 = (Visited,Info,ShootPos,Dontgo),
    Info = [Border,StartPoint,WumpusPos],
    %miss的话将其shootposition删去
    %接feedback后 发出指令
    (   
        WumpusPos == unknown ->
            (member(wumpus,Feedback) ->

                length(Feedback,WP),
                takeN(WP,Guess,PathToWumpus),

                %找出wumpus位置
                find(StartPoint,WumpusPosition,PathToWumpus), 
                NewInfo = [Border,StartPoint,WumpusPosition], %把wumpus位置加在info里
                getShootPositions(Border,WumpusPosition,ShootPositions), %得到shootpositions
                    
                %把wumpus位置加在Dontgo里
                append([WumpusPosition],Dontgo,NewDontgo),
                %State里存了刚放的shootPositions,加入了新的wumpusPosition
                State = (Visited,NewInfo,ShootPositions,NewDontgo),
                write(updateFinished),nl; 

                %Feedback没wumpus，这次没找到，记录path加入visited不要再去探索
                recordPath(StartPoint,Guess,UpdatedVisited,[]),
                append(UpdatedVisited,Visited,NewVisited),
                State = (NewVisited,Info,ShootPos,Dontgo)
                );
            
            %已知wumpus位置，且已尝试shoot，失败

            %Miss %将shootposition删去
            ShootPos = [_|RestShootPos],
            State = (Visited,Info,RestShootPos,Dontgo)

        ).
    /*
    0. 根据feedback，empty的就记录在visited，探索wumpus时就从剩下的找
    1. 遇到Pit和wall，根据feedback和guess,算出位置，记录其位置到State里（Dontgo）
    2. 改find，不要碰不要经过给出那些Dontgo位置
    */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%closestShootPosition(). 可能没必要

recordPath(_,[],UpdatedVisited,UpdatedVisited).
recordPath(StartPoint,[Direction|RestGuess],UpdatedVisited,A):-
    find(StartPoint,Stop,[Direction]),
    append([Stop],A,A1),
    recordPath(Stop,RestGuess,UpdatedVisited,A1).

getShootPath(StartPoint,(SX,SY),(WX,WY),Dontgo,ShootPath):-
    find(StartPoint,(SX,SY),Dontgo,ShootPath),
    %length(ShootPath,LenOfP),
    %Limit is SY + SY,
    %LenOfP =< Limit,
    checkShootPath(SX,WX,SY,WY,ShootPath).

checkShootPath(SX,WX,SY,WY,P):-    
    last(P,Move),
    (
        SX =:= WX ->
            (SY > WY ->
                Move = north;
                Move = south
                );
        SY =:= WY ->
            (SX < WX ->
                Move = east;
                Move = west
                )            
        ).

getShootPositions((NR,NC),(X,Y),ShootPositions):-
    shootPositionXLoop(NR,NC,(X,Y),A1,[]),
    shootPositionYLoop(NR,NC,(X,Y),A2,[]),
    append(A1,A2,A3),
    RedundancePos = [(X,Y),(X,1),(X,NC),(NR,Y),(1,Y)],
    subtract(A3,RedundancePos,ShootPositions),
    write(allShootPositionsAre),nl,
    write(ShootPositions),nl.

shootPositionXLoop(NR,NC,(X,Y),ShootPositions,A):-
    (   
        NR =:= 0 ->
            ShootPositions = A;
        NR > 0 ->
            append([(NR,Y)],A,A1),
            NNR is NR - 1,
            shootPositionXLoop(NNR,NC,(X,Y),ShootPositions,A1)
        ).
shootPositionYLoop(NR,NC,(X,Y),ShootPositions,A):-
    (   
        NC =:= 0 ->
            ShootPositions = A;
        NC > 0 ->
            append([(X,NC)],A,A1),
            NNC is NC - 1,
            shootPositionYLoop(NR,NNC,(X,Y),ShootPositions,A1)
        ).



% Take first N elements in the list
takeN(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
takeN(_, [], []).
takeN(N, [X|Xs], [X|Ys]) :- M is N-1, takeN(M, Xs, Ys).


%把不去的地方放到Previous就可以了
find(StartPoint, Destination, Path) :-
    find(StartPoint, Destination, [StartPoint], Path).
find(StartPoint, StartPoint, _, []).
find(StartPoint, Destination, Previous, [Direction|Path]) :-
    edge(StartPoint, Direction, Stop),
    \+ member(Stop, Previous),
    find(Stop, Destination, [Stop|Previous], Path).

buildMap([],_,_).
buildMap([(X,Y)|RestCords],NR,NC):-
    EX is X + 1,
    WX is X - 1,
    NY is Y - 1,
    SY is Y + 1,
    NNR is NR + 1,
    NNC is NC + 1,
    (
        WX > 0 ->
            aTob((X,Y),(WX,Y));
            write(1)
    ),
    (
        EX < NNR ->
            aTob((X,Y),(EX,Y));
            write(1)
    ),
    (    NY > 0 ->
            aTob1((X,Y),(X,NY));
            write(1)
    ),
    (    SY < NNC ->
            aTob1((X,Y),(X,SY));
            write(1)
    ),
    buildMap(RestCords,NR,NC).

getCords(X,Y,AllCords,A):-
    (   X =:= 0 ->
            AllCords = A;
        X > 0 ->
            getCordsYLoop(X,Y,A1,[]),
            NX is X - 1,
            append(A,A1,A2),
            getCords(NX,Y,AllCords,A2)
        ).
getCordsYLoop(X,Y,AllCords,A):-
    (   Y =:= 0 ->
            AllCords = A;
        Y > 0 ->
            append([(X,Y)],A,A1),
            NY is Y - 1,
            getCordsYLoop(X,NY,AllCords,A1)
        ).

aTob(A,B):-
    A = (X,Y),
    B = (BX,Y),
    (BX - X > 0 ->
    assert(edge(A,east,B));
    assert(edge(A,west,B))
    ).
aTob1(A,B):-
    A = (X,Y),
    B = (X,BY),
    (BY - Y > 0 ->
    assert(edge(A,south,B));
    assert(edge(A,north,B))
    ).
/*
limitSteps([],Ps,_,A):-
    delete(A,[],Ps).
limitSteps([Path|RestP],Ps,Limit,A):-
    length(Path,Len),
    (   Len =< Limit ->
        append([Path],A,A1);
        A1 = A
        ),
    limitSteps(RestP,Ps,Limit,A1).
    */

/*
selectShortestPath([],_,A,A).
selectShortestPath([Path|RestP],Len,ShortestP,A):-
    length(Path,Len1),
    (   Len1 > 0, Len1 < Len ->
            A1 = [Path],
            Len2 is Len1;
            Len1 =:= Len ->
                append([Path],A,A1),
                Len2 is Len1;
                Len2 = Len,
                A1 = A
        ),
    selectShortestPath(RestP,Len2,ShortestP,A1).
*/

/*
fAll(StartPoint,(SX,SY),L):-
    findall(P,find(StartPoint,(SX,SY),P),AllPs),
    length(AllPs,L).
    */


/*
getUnwantPaths(_,_,_,_,[],Paths,A):-
    delete(A,[],Paths).
getUnwantPaths(SX,WX,SY,WY,[P|RestP],Paths,A):-
    isUnwantPath(SX,WX,SY,WY,P,TrueP),
    append([TrueP],A,A1),
    getUnwantPaths(SX,WX,SY,WY,RestP,Paths,A1).
*/
