% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(wumpus,[initialState/5, guess/3, updateState/4]).
%TODO: 1.最后都跑不出wumpus的话会起始点循环
%****    改写find,和state


% Done
initialState(NR, NC, XS, YS, State0):-
    %calculate all coordinates
    getCords(NR,NC,Cords,[]),
    %initialState has all unexplored coordinates 
    append(Cords,[(XS,YS)],State0). 

guess(State0, State, Guess):-
    write(State0),
    %
    last(State0,CheckWumpus),
    (CheckWumpus == wumpus ->
        %Get WumpusPosition which is the 2nd last element in the list
        length(State0,N),
        NWumpus is N - 1,
        nth1(NWumpus,State0,WumpusPosition),
        % Shoot!
        WumpusPosition = (X,Y),
        SY = Y - 1,

        %Get StartPoint
        NStart is N - 2,
        nth1(NStart,State0,StartPoint),
        find(StartPoint,(X,SY),Path,_),
        append(Path,[shoot],Guess),
        State = State0
        %射中应该结束，然而继续就没射中，所以删去
        ;
        % miss? 换位置再射
        
        %下一个robot
        sort(State0,S1),
        last(S1,Destination), % Pick the bottom right coordinate as destination
        %也可改成去离出发点StartPoint最远的地点
        last(State0,StartPoint), % Get the StartPoint
        find(StartPoint,Destination,Guess,_),
        State = State0
        ).

    %
    

% Call after a robot got killed or finishied
updateState(State0, Guess, Feedback, State):-
    sort(State0,S1),
    last(S1,Destination), 
    last(State0,StartPoint), % Get the StartPoint
    %Remove all visited coordinates and get a new State
    find(StartPoint,Destination,Guess,Previous),
    subtract(State0,Previous,NewState),

%TODO:途中被吃，得算出第几步遇到wumpus然后算出它位置
%    途中没了得feedback后再清除state
    (member(wumpus,Feedback) ->
        nth1(EndPosition,Feedback,wumpus),
        getGuess(Guess,EndPosition,WumpusPosition),

        %Get wumpus position and send back to guess
        append(NewState,[Destination,wumpus],State); 
        State = NewState
    ),
    write(Feedback).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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