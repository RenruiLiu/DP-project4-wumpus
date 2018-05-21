

%% find a simple Path from Start to End

find(Start, End, Path) :-
	find(Start, End, [Start], Path).

find(Start, Start, _Previous, []).

find(Start, End, Previous, [Dirn|Path]) :-
    edge(Start, Dirn, Med),
    \+ member(Med, Previous), % dont visit previous places
    find(Med, End, [Med|Previous], Path).

%% map represented as facts
%% a - b - c - q
%%     |   |
%% k - d - e - f
%%     |   |   |
%%     g - h - i 
edge(a,  east, b).
edge(b,  west, a).
edge(b, south, d).
edge(b,  east, c).
edge(c,  west, b).
edge(c, south, e).
edge(d, north, b).
edge(d,  east, e).
edge(d, south, g).
edge(e,  west, d).
edge(e, north, c).
edge(e,  east, f).
edge(e, south, h).
edge(f,  west, e).
edge(f, south, i).
edge(g, north, d).
edge(h, north, e).
edge(h,  east, i).
edge(i,  west, h).
edge(i, north, f).
edge(g,  east, h).

aTob(A,K,B):-
    assert(edge(A,K,B)).

k(A,B):-
    B is A + 1.
gao(A,B,Y):-
    ( k(A,B) -> 
        Y = B;
        Y = 0
        ).
%还是找出一个Member再放去找路把




/*  if (没wumpus位置)
        if (wall)
            修正Guess ->TruePath DontGo1
            recordPath(StartPoint,TruePath,NewVisited,[]).
        else TruePath = Guess, DontGo1 = DontGo
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
            %?记录TruePath 入 NewVisited， 
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
            
        找不到path就删 (筛一下shootpos)
        finall(SPosition,getShootPath(StartPoint, NewShootPos, WumpusPosition,NewDontgo, _ShootPath, SPosition),NewShootPos2),

        State = (NewVisited, Info, NewShootPos2, NewDontgo)
*/

/*
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
                %筛选出可走的射击点
                findall(SPosition,getShootPath(StartPoint, ShootPositions, WumpusPosition,NewDontgo, _ShootPath, SPosition),NewShootPos),
                %State里存了刚放的shootPositions,加入了新的wumpusPosition
                State = (Visited,NewInfo,NewShootPos,NewDontgo),
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
*/