% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    State0 = (Visited,Info,ShootPositions).


guess(State0, State, Guess):-
    %find path 然后去走
    State0 = (_Visited,Info,ShootPositions),
    Info = [Border,StartPoint,WumpusPosition], %Info = [边界/wumpus, 起点]
    (   \+ WumpusPosition == unknown ->
        ShootPositions = [ShootPosition|_],
        write(ShootPosition),nl,

        %拿所有射击路线，然后选出可以击杀的路线
        findShootPaths(StartPoint,ShootPosition,Paths,[]),

        append(Path,[shoot],Guess), %应该找最短的
        State = State0;
        
        %不知道wumpus位置，继续探索
        find(StartPoint,Border,Guess), %一开始就去地图边界 %找没去过的地方
        State = State0 
    ).
        
updateState(State0, Guess, Feedback, State):-
    write(Feedback),nl,
    write(Guess),nl,
    State0 = (Visited,Info,ShootPos),
    Info = [Border,StartPoint,WumpusPos],
    %miss的话将其shootposition删去
    %接feedback后 发出指令
    (   
        WumpusPos == unknown ->
            (member(wumpus,Feedback) ->
                length(Feedback,WP),
                takeN(WP,Guess,PathToWumpus),
                find(StartPoint,WumpusPosition,PathToWumpus),
                NewInfo = [Border,StartPoint,WumpusPosition], %把wumpus位置加在info里
                getShootPositions(Border,WumpusPosition,ShootPositions), %把shootpositions放入state
                State = (Visited,NewInfo,ShootPositions),
                write(updateFinished),nl %需要在此update Visited
                );
            %已知wumpus位置，且已尝试shoot，失败

            %将shootposition删去
            ShootPos = [_|RestShootPos],
            State = (Visited,Info,RestShootPos)
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findShootPaths(StartPoint,ShootPosition,Paths,A):-   
    %要用到findall了
    find(StartPoint,ShootPosition,Path),

    getShootPositions((NR,NC),(X,Y),ShootPositions):-
    shootPositionXLoop(NR,NC,(X,Y),A1,[]),
    shootPositionYLoop(NR,NC,(X,Y),A2,[]),
    append(A1,A2,A3),
    RedundancePos = [(X,Y),(NR,NC),(1,NC),(1,1),(NR,1)],
    subtract(A3,RedundancePos,ShootPositions),
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
