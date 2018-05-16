%%:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% [Done 15 May 2018] Initialize game state
%% Game state is consist of
%%      -> A traversed Map (always from the output of updateState)
%%      -> A list of steps (planned to traverse)
%%      -> informations that is needed thoughout the guess, such as 
%%         map size and energy
initialState(NR,NC,XS,YS,[[XS,YS,empty],[XS-YS],NR,NC,100]).

%% Map [[X,Y,_]]-> Map construction from the last robot
%% Steps [X-Y]-> traversed blocks following the sequence of Guess
guess(State1,State2,Guess):-
    guess(State1,State2,[],Guess).

guess([OM,NS,NR,NC,0],[OM,NS,NR,NC,0],GuessHist,GuessHist).
guess([OldMap,[X1-Y1|Steps],NR,NC,EN1],[OldMap,NewSteps,NR,NC,EN2],GuessHist,Guess):-
(   EN1 > 0 ->  
        nextDes(NR,NC,[X1-Y1|Steps],XP,YP),
        findPath(X1,Y1,XP,YP,EN1,ENP,NR,NC,Guess1),
        constSteps(X1-Y1,NR,NC,EN1,Guess1,[X1-Y1|Steps],NewSteps1),
        append(GuessHist,Guess1,GuessHist2),
        write(EN1),write(' '),write(ENP),write(' '),write(Guess1),write('||'),
        guess([OldMap,NewSteps1,NR,NC,ENP],[OldMap,NewSteps,NR,NC,EN2],GuessHist2,Guess)

    ).



%% [Done 15 May 2018] X, Y that is not traversed
nextDes(NR,NC,Steps,X,Y):-
    random_between(1,NC,X),
    random_between(1,NR,Y),
    \+ member(X-Y,Steps).


%% [Done 15 May 2018] find a path and leftover energy given start and destination
findPath(X,Y,X1,Y1,EN,EN1,NR,NC,Path):-
    findPath(X,Y,X1,Y1,EN,EN1,NR,NC,[X-Y],Path).

findPath(_,_,_,_,0,0,_,_,_Hist,[]).
findPath(X,Y,X,Y,EN,EN,_,_,_Hist,[]).
findPath(X1,Y1,X2,Y2,EN,EN2,NR,NC,Hist,[NMove|Guess]):-
(   EN > 0 ->
        move(X1,Y1,XP,YP,EN,ENP,NR,NC,NMove),
        Step = XP-YP,
        \+ member(Step,Hist),
        findPath(XP,YP,X2,Y2,ENP,EN2,NR,NC,[Step|Hist],Guess)
    ).

%% [Done 15 May 2018] able to construct list of traversed blocks using
%% set of instructions
constSteps(_,_,_,_,[],OldSteps,OldSteps).
constSteps(X-Y,NR,NC,EN,[Move|Guess],OldSteps,NewSteps):-
    move(X,Y,X1,Y1,EN,EN1,NR,NC,Move),
    ( member(X1-Y1,OldSteps) ->
        keep(OldSteps,NewSteps1)
    ;   cont(X1,Y1,OldSteps,NewSteps1)
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


%%updateState([X,Y,_,],Guess,Feedback,State1):-

cont(X,Y,Step,[X-Y|Step]).
keep(Step,Step).

myL(0,[]).
myL(N,[_|List]):-
    N1 is N + 1,
    myL(N1,List).

myLength(0,[]).
myLength(N,[H|L]):-
    myL(Add,H),
    myLength(N1,L),
    N is Add + N1.

    