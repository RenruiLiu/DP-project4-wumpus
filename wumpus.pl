% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO: 
%       1.所有DontGo改成Avoid
%       2.find老问题，大地图 (1.找射击路径，2.找猜测路径)

:- module(wumpus,[initialState/5, guess/3, updateState/4]).

initialState(NR, NC, XS, YS, State0):-
    %calculate all coordinates
    getCords(NC,NR,Cords,[]),
    %build Facts
    buildMap(Cords,NC,NR),
    Visited = [(XS,YS)],
    WumpusPosition = unknown,
    Info = [(NC,NR),(XS,YS),WumpusPosition],
    ShootPositions = [],
    Dontgo = [(XS,YS)],
    State0 = (Visited,Info,ShootPositions,Dontgo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

guess(State0, State, Guess):-
    State0 = (Visited,Info,ShootPositions,Dontgo1),
    Info = [Border,StartPoint,WumpusPosition],
    sort(Dontgo1,Dontgo), % delete duplicates

    (   \+ WumpusPosition == unknown ->
        write(进入射击),nl,
    % try to shoot 
        random_permutation(ShootPositions,RandomShootPosLst),
        member(ShootPos,RandomShootPosLst),
        write(射击点是),nl,
        write(ShootPos),nl,
    % 射击和起始点一样
        (   StartPoint == ShootPos ->
            % if the shoot position is the start point
                write(射击点一样),nl,
                getShootPath2(StartPoint,WumpusPosition,Visited,Dontgo,SPath),
                write(这里过了),nl;   
            (   getShootPath1(StartPoint,ShootPos,WumpusPosition,Dontgo,SPath1)
                 ->
                    SPath = SPath1,
                    NewShootPos = ShootPositions;
                % if can't find a path to this shoot position, delete it
                    delete(ShootPositions,ShootPos,NewShootPos)
            ), 
            getShootPath1(StartPoint,ShootPos,WumpusPosition,Dontgo,SPath)
        ),
        append(SPath,[shoot],Guess),
        State = (Visited,Info,NewShootPos,Dontgo),
        write(射击出去),nl
        ;
        
    % Don't know Wumpus position, keep exploring
        write(进入explore),nl,
        Border = (NR,NC),
        getCords(NR,NC,Cords,[]),
        subtract(Cords,Visited,UnVisitedLst),
    % explore those UnVisited places 
        write(探索目的地包括),nl,
        write(UnVisitedLst),nl,
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
        % get the wall positions and 
        % what are the coordinates just visited
            getWallPositions(StartPoint,Feedback,LenGuess,WallPositions,[]),
            limit(1,getTruePath(Feedback,LenGuess,TruePath)),  
            recordPath(StartPoint,TruePath,TrueVisited,[]),
        % Append the path and wall position into Visited, and wall into Dontgo
            append(TrueVisited,WallPositions,WallUpdateVisited),
            append(WallUpdateVisited,Visited,NewVisited),
            append(WallPositions,Dontgo,Dontgo1),
            write(wall出去了),nl
            ;

        % Didn't meet any walls
            TruePath = LenGuess,
            Dontgo1 = Dontgo,
            recordPath(StartPoint,TruePath,NewVisited1,[]),
            append(NewVisited1,Visited,NewVisited)
            ),

    % Fell in a pit
        (member(pit,Feedback) ->
            write(掉进pit),nl,
        % Add Pit to Dontgo
            backTrack(StartPoint,TruePath,PitPosition),
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
        % Calculate all shoot Positions
            getShootPositions(Border,WumpusPosition,AllShootPos),
            State = (NewVisited, NewInfo, AllShootPos, NewDontgo),
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
            limit(1,getTruePath(Feedback,Guess1,TruePath)),        
        % Append the path and wall position to Visited, and wall to Dontgo
            recordPath(StartPoint,TruePath,TrueVisited,[]),
            append(TrueVisited,WallPositions,WallUpdateVisited),
            append(WallUpdateVisited,Visited,NewVisited),
            append(WallPositions,Dontgo,Dontgo1);

        % Didn't meet any walls
            TruePath = Guess1,
            Dontgo1 = Dontgo,
            recordPath(StartPoint,TruePath,NewVisited1,[]),
            append(NewVisited1,Visited,NewVisited)
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
        % delete this shoot position
            delete(NewShootPos,IdealShootPos,NewShootPos1);
            NewShootPos1 = NewShootPos
            ),

        State = (NewVisited, Info, NewShootPos1, NewDontgo),
        write(从updateState出去了3),nl
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete moves those hit walls from Guess
getTruePath(Feedback,Guess,TruePath):-
    (
        member(wall,Feedback) ->
            nth1(W,Feedback,wall,RestFeedback),
            nth1(W,Guess,_,RestGuess),
            getTruePath(RestFeedback,RestGuess,TruePath);
        TruePath = Guess
        ).

getWallPositions(StartPoint,Feedback,Guess,WallPositions,A):-
    findall(W,(nth1(W,Feedback,wall)),Walls),
    (Walls == [] ->
        WallPositions = A;
    % Get this wall Position
        Walls = [Wall|_],
        takeN(Wall,Guess,PathToWall),
    % Cut PathToWall to get RestGuess
        append(PathToWall,RestGuess,Guess),
    % Cut First walls feedback to get the feedback for rest walls
        takeN(Wall,Feedback,FstFeedback),
        append(FstFeedback,RestFeedback,Feedback),
    % Get new StartPoint after the robot hit a wall
        Wall1 is Wall - 1,
        takeN(Wall1,Guess,PathToNewStartPoint),
        backTrack(StartPoint,PathToNewStartPoint,NewStartPoint),
    % See if it hit the Border of the map
        (find(StartPoint,WallPos,PathToWall) ->
            WallPosition = WallPos,
            append([WallPosition],A,A1),
            getWallPositions(NewStartPoint,RestFeedback,RestGuess,WallPositions,A1)
            ;
        % Hit the Border, Dont record that
            getWallPositions(NewStartPoint,RestFeedback,RestGuess,WallPositions,A)
            )
        ).

% gather all coordinates in the path
recordPath(_,[],UpdatedVisited,UpdatedVisited).
recordPath(StartPoint,[Direction|RestGuess],UpdatedVisited,A):-
    backTrack(StartPoint,[Direction],Stop),
    append([Stop],A,A1),
    recordPath(Stop,RestGuess,UpdatedVisited,A1).

% find a path to shoot positions
getShootPath1(StartPoint,ShootPos,(WX,WY),Dontgo,ShootPath):-
    find(StartPoint,ShootPos,Dontgo,ShootPath),
    ShootPos = (SX,SY),
    checkShootPath(SX,WX,SY,WY,ShootPath).

% if the shoot position is the startpoint
getShootPath2(StartPoint,WumpusPosition,Visited,Dontgo,ShootPath):-
    subtract(Visited, Dontgo, Visited1),
    delete(Visited1, StartPoint, Visited2),
    delete(Dontgo,StartPoint,Dontgo1),
% find a random visited coordinate
    random_permutation(Visited2,RandomVisited),
    member(V1,RandomVisited),
    find(StartPoint,V1,Dontgo,Path1),
    find(V1,StartPoint,Dontgo1,Path2),
    StartPoint = (SX,SY),
    WumpusPosition = (WX,WY),
    checkShootPath(SX,WX,SY,WY,Path2),
% go to that random visited coordinate
% and go back to the shoot position
    append(Path1,Path2,ShootPath).

% check this shoot path
checkShootPath(SX,WX,SY,WY,P):-    
    last(P,Move),
    % make sure the robot is facing to wumpus
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
% get all coordinates that can shoot wumpus in the map
    append(A1,A2,A3),
    RedundancePos = [(X,Y),(X,1),(X,NC),(NR,Y),(1,Y)],
% delete those are on the border of the map
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

% It's the same to the find predicate in search.pl
find(StartPoint, Destination, Path) :-
    find(StartPoint, Destination, [StartPoint], Path).
find(StartPoint, StartPoint, _, []).
find(StartPoint, Destination, Previous, [Direction|Path]) :-
    edge(StartPoint, Direction, Stop),
    \+ member(Stop, Previous),
    find(Stop, Destination, [Stop|Previous], Path).

% track the Destination
backTrack(StartPoint,[],StartPoint).
backTrack(StartPoint,[Direction|Path],Destination):-
    edge(StartPoint, Direction,Stop),
    backTrack(Stop, Path, Destination).

% assert facts for every coordinate in the map
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

% get all coordinates in the map
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

% explore the map untill find wumpus
exploreMap(StartPoint,UnVisitedLst,Dontgo,Visited,NewVisited,Guess):-
% pick a random Unvisited coordinate    
    random_permutation(UnVisitedLst,RandomLst),
    member(UnVisited,RandomLst),
    (
        write(探索目的地是),nl,
        write(UnVisited),nl,
        write(卡在这),nl, find(StartPoint,UnVisited,Dontgo,Path) ->
            Guess = Path,
            NewVisited = Visited
            ;
    % coordinates that the robot can't find a path to
    % will be transfered to Visited list
        append([UnVisited],Visited,NewVisited)
        ).
