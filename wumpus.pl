% Declarative Programming Project 4

% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus

% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au


:- module(wumpus,[initialState/5, guess/3, updateState/4]).

% Done
initialState(NR, NC, XS, YS, State0):-
    BorderR is NR + 1,
    BorderC is NC + 1,
    bor(BorderR,BorderC,List), % The borders should be larger than the map size by 1.
    append(List,[(XS,YS)],State),
    sort(State,State0), % Remove depulicates
    write(State0).

guess(State0, State, Guess):- 
    action(A),
    Guess = [A],
    move(State0,A,NewState),
    append([NewState],State0,State1),
%    \+ member(NewState,State),
%    append([NewState],State,State1),
    State is State1.
%    guess(NewState,State1,Guess).

%    Guess = [east,east,east,south,shoot].

updateState(State0, Guess, Feedback, State).


action(west).
action(south).
action(east).
action(north).

move( (X,Y) , Action , NewPosition):-
  (   Action == west ->
        NewX is X - 1,
        NewPosition = (NewX,Y);
        Action == east ->
            NewX is X+1,
            NewPosition = (NewX,Y);
            Action == north ->
                NewY is Y+1,
                NewPosition = (X,NewY);
                NewY is Y-1,
                NewPosition = (X,NewY)
            ).

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