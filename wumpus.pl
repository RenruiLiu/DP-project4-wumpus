% Declarative Programming Project 4
%
% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus
%
% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO:  1.思考改Update结构

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
    write(看State0),nl,
    write(Visited),nl,
    write(真的是State),nl,
    (   \+ WumpusPosition == unknown ->

        %拿所有射击路线，然后选出（一条）可以击杀的路线
        getShootPath(StartPoint,ShootPositions,WumpusPosition,Dontgo,Path),
        append(Path,[shoot],Guess),
        State = State0
        ;
        
        %不知道wumpus位置，继续探索 ！没去的地方

        Border = (NR,NC),
        getCords(NR,NC,Cords,[]),
        subtract(Cords,Visited,AllUnVisited),
        write(firsttttttttttstartsHere),nl,
        write(StartPoint),nl,
        findPath(StartPoint,AllUnVisited,Dontgo,Guess),
        %find(StartPoint,UnVisited,Dontgo,Guess),
        State = State0 
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        

updateState(State0, Guess, Feedback, State):-
% miss的话将其shootposition删去
    write(Guess),nl,
    write(Feedback),nl,

    State0 = (Visited,Info,ShootPos,Dontgo),
    Info = [Border,StartPoint,WumpusPos],
    
/*  if (没wumpus位置)
        if (wall)
            修正Guess ->TruePath
            recordPath(StartPoint,TruePath,NewVisited,[]).
        else TruePath = Guess
            recordPath(StartPoint,TruePath,NewVisited,[]).

        if (pit)
            TruePath -> pitPoistion -> 加入NewDontgo
            subtract(ShootPos, NewDontgo, NewShootPos),
            State = (NewVisited, Info, NewShootPos, NewDontgo)
        else write(noPit)

        if (Wumpus)
            TruePath，shootposition -> 加入NewShootPos, 加入NewDontgo 和 NewInfo
            subtract(ShootPos, NewDontgo, NewShootPos1),
            State = (NewVisited, NewInfo, NewShootPos1, NewDontgo)
        else write(noWumpus)

        if (no pit no wumpus)
            记录TruePath 入 NewVisited， 
            State = (NewVisited, Info, ShootPos, Dontgo)
        else write(foundSomething)
        
    else (知道位置,已经试着去射，失败而归) Guess最后一位是shoot
        delete(Guess,shoot,Guess1),
        if (wall)
            修正Guess1 -> TruePath
            recordPath(StartPoint,TruePath,NewVisited,[]).
            else TruePath = Guess1
            recordPath(StartPoint,TruePath,NewVisited,[]).

        last(NewVisited,TrueShootPos),
        （位置对方向对，然而miss）
        if (TrueShootPos == ShootPos && checkShootPath(SX,WX,SY,WY,TruePath) )
                删去position -> NewShootPos

        else (没到射击地)
            if (pit) 
                TruePath -> pitPoistion -> 加入NewDontgo;
                subtract(ShootPos, NewDontgo, NewShootPos),

                %State = (NewVisited, NewInfo, NewShootPos, NewDontgo)
            else NewDontgo = Dontgo
                NewShootPos = ShootPos
            
        ??试一下能不能找到ShootPath，能就去，不能就删 (筛一下shootpos)
        getShootPath(StartPoint, NewShootPos, WumpusPosition,NewDontgo, ShootPath),

        State = (NewVisited, Info, NewShootPos2, NewDontgo)
*/

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  The robot just found wumpus.
            (member(wumpus,Feedback) ->
                write(进入了刚找到wumpus),nl,
                %%找出wumpus位置
                length(Feedback,WP),
                takeN(WP,Guess,PathToWumpus),
                find(StartPoint,WumpusPosition,PathToWumpus), 
                %%把wumpus位置加在info和DontGo里
                NewInfo = [Border,StartPoint,WumpusPosition], 
                append([WumpusPosition],Dontgo,NewDontgo),
                %得到shootpositions
                getShootPositions(Border,WumpusPosition,ShootPositions), 
                %State里存了刚放的shootPositions,加入了新的wumpusPosition
                State = (Visited,NewInfo,ShootPositions,NewDontgo),
                write(updateFinished0),nl
                ; 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Found wall
        %1.收集所有wall和位置到list
        %3.把wall从guess中删去，得到真实的guess可以算出path,加入visited里
        %2.放入visited和dontgo
            (member(wall,Feedback)) ->
                write(进入了wall),nl,
                getWallPositions(StartPoint,Feedback,Guess,WallPositions,[]),
                write(wall2),nl,
            %% Get true Path (Dont know why it returns 2 same results)
                findall(TruePaths,getTruePath(Feedback,Guess,TruePaths),AllTruePaths),
                last(AllTruePaths,TruePath),
                write(wall3),nl,
            %% Append the path and wall position into Visited, and wall into Dontgo
                write(StartPoint),nl,
                write(TruePath),nl,
                recordPath(StartPoint,TruePath,TrueVisited,[]),
                write(wall4),nl,
                append(TrueVisited,WallPositions,WallUpdateVisited),
                append(WallUpdateVisited,Visited,NewVisited),
                
                append(WallPositions,Dontgo,NewDontgo),
                write(WallUpdateVisited),
                State = (NewVisited,Info,ShootPos,NewDontgo),
                write(从wall出去了),nl
                ;
                                

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Found pit 
            (member(pit,Feedback)) ->
                write(进入了pit),nl,

                length(Feedback,Pit),
                takeN(Pit,Guess,PathToPit),                
                find(StartPoint,PitPosition,PathToPit),
        %%  Add Pit to Dontgo and PathToPit to Visited
                append([PitPosition],Dontgo,NewDontgo),
                recordPath(StartPoint,PathToPit,UpdatedVisited,[]),
                append(UpdatedVisited,Visited,NewVisited1),
                sort(NewVisited1,NewVisited),%Remove duplicates
                State = (NewVisited,Info,_ShootPositions,NewDontgo),
                write(updateFinished1),nl,
                write(NewVisited),nl,
                write(dontGoAre),nl,
                write(NewDontgo),nl
                ;

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  No wumpus, 记录path加入visited不要再去探索
                write(进入了啥都没),nl,

                recordPath(StartPoint,Guess,UpdatedVisited,[]),
                append(UpdatedVisited,Visited,NewVisited),
                State = (NewVisited,Info,ShootPos,Dontgo)
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


getWallPositions(StartPoint,Feedback,Guess,WallPositions,A):-
    findall(W,(nth1(W,Feedback,wall)),Walls),
    (Walls == [] ->
        WallPositions = A;

        %%Get this wall Position
        Walls = [Wall|_],
        takeN(Wall,Guess,PathToWall),
        find(StartPoint,WallPosition,PathToWall),
        append([WallPosition],A,A1),

    %% Cut PathToWall to get RestGuess
        append(PathToWall,RestGuess,Guess),
    %% Cut First walls feedback to get feedback for rest walls
        takeN(Wall,Feedback,FstFeedback),
        append(FstFeedback,RestFeedback,Feedback),
    %% Get new StartPoint after the robot hit a wall
        Wall1 is Wall - 1,
        takeN(Wall1,Guess,PathToNewStartPoint),
        find(StartPoint,NewStartPoint,PathToNewStartPoint),
        getWallPositions(NewStartPoint,RestFeedback,RestGuess,WallPositions,A1)
        ).

findPath(StartPoint,AllPositions,Dontgo,Guess):-
    write(进入findPath),nl,
    write(whereToVisit),nl,
    write(startsHere),nl,
    write(StartPoint),nl,
    write(hereareDontgo),nl,
    write(Dontgo),nl,
    write(allPositionsAre),nl,
    write(AllPositions),nl,
    member(Position,AllPositions),
    write(Position),nl,
    find(StartPoint,Position,Dontgo,Guess).

recordPath(_,[],UpdatedVisited,UpdatedVisited).
recordPath(StartPoint,[Direction|RestGuess],UpdatedVisited,A):-
    (   Direction == shoot ->
            recordPath(Stop,[],UpdatedVisited,A1)
        ;
    find(StartPoint,Stop,[Direction]),
    append([Stop],A,A1),
    recordPath(Stop,RestGuess,UpdatedVisited,A1)
    ).

getShootPath(StartPoint,ShootPositions,(WX,WY),Dontgo,ShootPath):-
    write(这里吗),nl,
    findPath(StartPoint,ShootPositions,Dontgo,ShootPath),
    find(StartPoint,(SX,SY),ShootPath),
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

