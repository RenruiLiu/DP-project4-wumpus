% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO: 
%       2.所有DontGo改成Avoid
%       3.getwallposition有问题，不存在的wall加入了Dontgo

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

guess(State0, State, Guess):-
    %find path 然后去走
    State0 = (Visited,Info,ShootPositions,Dontgo),
    Info = [Border,StartPoint,WumpusPosition],

    (   \+ WumpusPosition == unknown ->
        write(进入射击),nl,
        member(SPos1,ShootPositions),
        (   getShootPath1(StartPoint,SPos1,WumpusPosition,Dontgo,SPath1) ->
                SPath = SPath1,
                write(射击点是),nl,
                write(SPos1),nl,
                NewShootPos = ShootPositions;
                delete(ShootPositions,SPos1,NewShootPos)
            ),
        getShootPath1(StartPoint,SPos1,WumpusPosition,Dontgo,SPath),
        append(SPath,[shoot],Guess),
        State = (Visited,Info,NewShootPos,Dontgo),
        %
        write(射击出去),nl
        ;
        
    % Don't know the position of Wumpus, keep exploring
        write(进入explore),nl,
        Border = (NR,NC),
        getCords(NR,NC,Cords,[]),
        subtract(Cords,Visited,UnVisitedLst),
        write(探索目的地包括),nl,
        write(UnVisitedLst),nl,

    % 想办法在fail后把fail的目的地加入Visited
        exploreMap(StartPoint,UnVisitedLst,Dontgo,Visited,NewVisited,Guess),
        State = (NewVisited,Info,ShootPositions,Dontgo),
        write(explore出去),nl
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        

updateState(State0, Guess, Feedback, State):-
    write(Guess),nl,
    write(Feedback),nl,
    State0 = (Visited,Info,ShootPos,Dontgo),
    Info = [Border,StartPoint,WumpusPos],

    % Cut the path based on the length of feedback
    length(Feedback,Len),
    takeN(Len,Guess,LenGuess),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    (WumpusPos == unknown ->

    % Hit a wall
        (member(wall,Feedback) ->
            write(进入wall),nl,
            getWallPositions(StartPoint,Feedback,LenGuess,WallPositions,[]),
            limit(1,getTruePath(Feedback,LenGuess,TruePath)),  
        % Append the path and wall position into Visited, and wall into Dontgo
            recordPath(StartPoint,TruePath,TrueVisited,[]),
            append(TrueVisited,WallPositions,WallUpdateVisited),
            append(WallUpdateVisited,Visited,NewVisited),
            append(WallPositions,Dontgo,Dontgo1),
            write(wall出去了),nl
            ;

        % Didn't meet any walls
            TruePath = LenGuess,
            Dontgo1 = Dontgo,
            recordPath(StartPoint,TruePath,NewVisited,[])
            ),

    % Fell in a pit
        (member(pit,Feedback) ->
            write(掉进pit),nl,  
            find(StartPoint,PitPosition,TruePath),
        % Add Pit to Dontgo
            append([PitPosition],Dontgo1,NewDontgo),
            State = (NewVisited, Info, ShootPos, NewDontgo),
            write(pit出来了),nl
            ;
            write(noPits),nl
            ),

    % Found Wumpus
        (member(wumpus,Feedback) ->
            write(找到wumpus了),nl,
            backTrack(StartPoint,TruePath,WumpusPosition),
        % Add WumpusPosition into info and DontGo
            NewInfo = [Border,StartPoint,WumpusPosition],
            append([WumpusPosition],Dontgo1,NewDontgo),
        % Get all shoot Positions
            getShootPositions(Border,WumpusPosition,AllShootPos), 
            subtract(AllShootPos,NewDontgo,ShootPositions),
            State = (NewVisited, NewInfo, ShootPositions, NewDontgo),
            write(wumpus出去了),nl
            ;
            write(noWumpus),nl
            ),

    % Didn't meet any pits or wumpus
        (\+ member(wumpus,Feedback) , \+ member(pit,Feedback) ->
            write(没wumpus没pit),nl,
            sort(NewVisited,SortVisited),
            State = (SortVisited, Info, ShootPos, Dontgo1),
            write(从updateState出去了1),nl;
            %有wumpus或者pit，但state都已经被处理了
            write(从updateState出去了2),nl
            )
        ;
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Know wumpus position and tried to shoot him but fail
        delete(LenGuess,shoot,Guess1),

    % Hit a wall
        (member(wall,Feedback) ->
            write(在射击途中撞墙),nl,
            getWallPositions(StartPoint,Feedback,Guess1,WallPositions,[]),
            limit(1,getTruePath(Feedback,LenGuess,TruePath)),          
        % Append the path and wall position to Visited, and wall to Dontgo
            recordPath(StartPoint,TruePath,TrueVisited,[]),
            append(TrueVisited,WallPositions,WallUpdateVisited),
            append(WallUpdateVisited,Visited,NewVisited),
            append(WallPositions,Dontgo,Dontgo1);

        % Didn't meet any walls
            TruePath = Guess1,
            Dontgo1 = Dontgo,
            recordPath(StartPoint,TruePath,NewVisited,[])
            ),

    % Fell into a pit
        (member(pit,Feedback) ->
            backTrack(StartPoint,TruePath,PitPosition),
        % Add Pit to Dontgo
            append([PitPosition],Dontgo1,NewDontgo),
            subtract(ShootPos, NewDontgo, NewShootPos);

        % Didn't meet any pits
            NewDontgo = Dontgo1,
            NewShootPos = ShootPos
            ),

    % Position and Direction are right, but missed (A wall in between)
        NewVisited = [TrueShootPos|_],
        delete(Guess,shoot,Guess0),
        backTrack(StartPoint,Guess0,IdealShootPos),
        IdealShootPos =  (SX,SY),
        WumpusPos = (WX,WY),
        (TrueShootPos == IdealShootPos, checkShootPath(SX,WX,SY,WY,TruePath) ->
            write(删射击点了),nl,
            delete(NewShootPos,IdealShootPos,NewShootPos1);
            NewShootPos1 = NewShootPos
            ),

        State = (NewVisited, Info, NewShootPos1, NewDontgo),
        write(从updateState出去了3),nl
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getTruePath(Feedback,Guess,TruePath):-
    (
        member(wall,Feedback) ->
            nth1(W,Feedback,wall,RestFeedback),
            nth1(W,Guess,_,RestGuess),
            getTruePath(RestFeedback,RestGuess,TruePath);
        TruePath = Guess
        ).

%这块有问题，不存在的wall加入了Dontgo
getWallPositions(StartPoint,Feedback,Guess,WallPositions,A):-
    findall(W,(nth1(W,Feedback,wall)),Walls),
    (Walls == [] ->
        WallPositions = A;
        %%Get this wall Position
        Walls = [Wall|_],
        takeN(Wall,Guess,PathToWall),
    % Cut PathToWall to get RestGuess
        append(PathToWall,RestGuess,Guess),
    % Cut First walls feedback to get feedback for rest walls
        takeN(Wall,Feedback,FstFeedback),
        append(FstFeedback,RestFeedback,Feedback),
    % See if it hit the Border of the map
        (find(StartPoint,WallPos,PathToWall) ->
            WallPosition = WallPos,
            append([WallPosition],A,A1),
        % Get new StartPoint after the robot hit a wall
            Wall1 is Wall - 1,
            takeN(Wall1,Guess,PathToNewStartPoint),
            backTrack(StartPoint,PathToNewStartPoint,NewStartPoint),
            getWallPositions(NewStartPoint,RestFeedback,RestGuess,WallPositions,A1),
            write(这里是wall的坐标),nl,
            write(WallPositions),nl
            ;
        % Hit the Border, Dont record that
            getWallPositions(StartPoint,RestFeedback,RestGuess,WallPositions,A)
            )
        ).

recordPath(_,[],UpdatedVisited,UpdatedVisited).
recordPath(StartPoint,[Direction|RestGuess],UpdatedVisited,A):-
    backTrack(StartPoint,[Direction],Stop),
    append([Stop],A,A1),
    recordPath(Stop,RestGuess,UpdatedVisited,A1).

getShootPath1(StartPoint,ShootPos,(WX,WY),Dontgo,ShootPath):-
    find(StartPoint,ShootPos,Dontgo,ShootPath),
    ShootPos = (SX,SY),
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
    subtract(A3,RedundancePos,ShootPositions).

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

backTrack(StartPoint,[],StartPoint).
backTrack(StartPoint,[Direction|Path],Destination):-
    edge(StartPoint, Direction,Stop),
    backTrack(Stop, Path, Destination).

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

exploreMap(StartPoint,UnVisitedLst,Dontgo,Visited,NewVisited,Guess):-
    random_permutation(UnVisitedLst,RandomLst),
    member(UnVisited,RandomLst),
    (
        write(探索目的地是),nl,
        write(UnVisited),nl,
        find(StartPoint,UnVisited,Dontgo,Path) ->
            Guess = Path,
            NewVisited = Visited
            ;
        append([UnVisited],Visited,NewVisited)
        ).

