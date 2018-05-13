% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO:
% 1.好像目前来讲，border不需要，因为只是在map的coordinates里走，不碰到边界，但暂时不删
% 2.机器人从A到B只有一条线路走
% 3.写updateState让机器人能重复开拓地图
% 4.找到wumpus后调整开枪

:- module(wumpus,[initialState/5, guess/3, updateState/4]).

% Done
initialState(NR, NC, XS, YS, State0):-
    BorderR is NR + 1,
    BorderC is NC + 1,
    bor(BorderR,BorderC,List), % The borders should be larger than the map size by 1.
    sort(List,List0),% Remove depulicates
    append(List0,[(XS,YS)],State0).

guess(State0, State, Guess):- 
    %Get all coordinates of the map and pick up all possible destinations
    allcoordinates(State0,Allcoordinates),
    write(State0),
    exploreMap(Allcoordinates,State0,Destinations),
    member(Destination,Destinations), % Pick one possible destination
    last(State0,StartPoint), % Get the StartPoint
    find(StartPoint,Destination,Path,Previous),
    append(State0,Previous,State1), 
    sort(State1,State), % Remove depulicates and get new state
    Guess = Path.
    %append(Path,[shoot],Guess). %

% Call after a robot got killed or finishied. %Not start yet
updateState(State0, Guess, Feedback, State):-
    last(State0,StartPoint), % Get the StartPoint
    move(StartPoint,Guess,Previous,[]),
    append(State0,Previous,State).
    %write(Feedback).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%remove all visited coordinates from allcoordinates list.
exploreMap(AllCords,State0,Destinations):-
    (   State0 == [] ->
            AllCords = Destinations;
            State0 = [Head|Tail],
            delete(AllCords,Head,D1),
            exploreMap(D1,Tail,Destinations)
    ).

allcoordinates(State0,AllCords):-
    length(State0,N),
    N1 is N - 1,
    nth1(N1,State0,A),
    A = (X,Y), % Get (NR,NC),which is at the snd last element
    getCords(X,Y,AllCords,[]).

getCords(X,Y,AllCords,A):-
    (   X =:= 0 ->
            AllCords = A;
        X > 0 ->
            yLoop(X,Y,A1,[]),
            NX is X - 1,
            append(A,A1,A2),
            getCords(NX,Y,AllCords,A2)
        ).
yLoop(X,Y,AllCords,A):-
    (   Y =:= 0 ->
            AllCords = A;
        Y > 0 ->
            append([(X,Y)],A,A1),
            NY is Y - 1,
            yLoop(X,NY,AllCords,A1)
        ).


%方法2，算End和Start差距，然后走
%算出在给定范围内走到End的所有方式，并且不重复
%Functions for calculating how many path that
% can move a robot from coordinate A to coordinate B

find(Start, Start, [],[]).
find((X,Y), (XS,YS), Path, Previous):-
    XD is XS - X,
    YD is YS - Y,
    xPath(XD,P1,[]),
    yPath(YD,P2,[]),
    append(P1,P2,Path), %A到B只有一条Path，需要多Paths?
    move((X,Y),Path,Previous,[]).

move( Start, Path, Previous,A):-
    (   Path == [] ->
            Previous = A;
        Path = [Action|Rest],
        move1(Start,Action,NewPosition),
        append([NewPosition],A,A1),
        move(NewPosition,Rest,Previous,A1)
        ).

move1( (X,Y) , Action , NewPosition):-
  (   Action == west ->
        NewX is X - 1,
        NewPosition = (NewX,Y);
        Action == east ->
            NewX is X+1,
            NewPosition = (NewX,Y);
            Action == north ->
                NewY is Y-1,
                NewPosition = (X,NewY);
                NewY is Y+1,
                NewPosition = (X,NewY)
).

xPath(XD,Path,A):-    
    (   XD =:= 0 ->
            Path = A;
            XD > 0 ->
            append([east],A,A1),
            NXD is XD - 1,
            xPath(NXD,Path,A1);
                append([west],A,A1),
                NXD is XD + 1,
                xPath(NXD,Path,A1)
    ).
yPath(YD,Path,A):-    
    (   YD =:= 0 ->
            Path = A;
            YD > 0 ->
            append([south],A,A1),
            NYD is YD - 1,
            yPath(NYD,Path,A1);
                append([north],A,A1),
                NYD is YD + 1,
                yPath(NYD,Path,A1)
    ).


/* 
action((X,Y),west,(NX,Y)):-
    NX is X - 1.
action((X,Y),south,(X,NY)):-
    NY is Y + 1.
action((X,Y),east,(NX,Y)):-
    NX is X + 1.
action((X,Y),north,(X,NY)):-
    NY is Y - 1.
*/


% Functions for setting borders of the map
bor(NR,NC,List):-
    borders(NR,NC,[],List1),
    borders1(NR,NC,[],List2),
    borders(NR,0,[],List3),
    borders1(0,NC,[],List4),
    append(List1,List2,ListX),
    append(List3,List4,ListY),
    append(ListX,ListY,List).

borders(NR,NC,A,List):- 
(   NR =:= -1 ->
        List = A;
    NR > -1,
        append([(NR,NC)], A, A1),
        NewNR is NR - 1,
        borders(NewNR,NC,A1,List)
).
borders1(NR,NC,A,List):- 
(   NC =:= -1 ->
        List = A;
    NC > -1,
        append([(NR,NC)], A, A1),
        NewNC is NC - 1,
        borders1(NR,NewNC,A1,List)
).