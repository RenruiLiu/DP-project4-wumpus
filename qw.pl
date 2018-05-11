
su(A,B,AB):-
    AB = (A,B).


action(west).
action(south).
action(east).
action(north).

q([A|R]):-
    (A == west ->
        write("OK");
        write("NO")
        ),
    (A == east ->
        write("OK");
        write("NO")
        ).
 k((A,B),X):-
    X = (A+1,B+1).