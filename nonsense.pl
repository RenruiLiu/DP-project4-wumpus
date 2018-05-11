
su(A,B,AB):-
    AB = (A,B).


action(west).
action(south).
action(east).
action(north).

q([A|_]):-
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




ini(NR,NC, XS, YS, State0):- 
    NNR is NR + 1,
    NNC is NC + 1,
    bor(NNR,NNC,List),
    append(List,[(XS,YS)],State),
    sort(State,State0).

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




cd(N):-
    (N>0 ->
    write(N),
    NewN is N-1,
    cd(NewN);
    write("finished")
    ).
