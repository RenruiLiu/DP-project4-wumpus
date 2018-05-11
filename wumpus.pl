% Declarative Programming Project 4

% This program is for playing wumpus game 
% which is basically sending robots to hunt wumpus

% By Renrui Liu, SID 950392, renruil@student.unimelb.edu.au


:- module(wumpus,[initialState/5, guess/3, updateState/4]).

initialState(NR, NC, XS, YS, State0):-
    State0 = [(NR,0),(0,NC),(NR,NC),(XS,YS)],%要改，从（0，0）到（NR,0）到（0,NC）再到（NR,NC）都进state里
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