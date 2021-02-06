:- module(run, [run/0]).
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(lists)).
run:-
	initial().

initial():-
    E1 = 31,
    E2 = 27,
    E3 = 6,
    E4 = 18,
    E5 = 31,
	new(Pict, window('Elevators logic',size(1100,350))),
    send(Pict, display, new(T, tabular)),
        send(T, border, 0),
        send(T, cell_spacing, -1),
        send(T, rules, all),
        send_list(T,
                  [ append(''),
                    append(''),
                    append('Pozycje startowe', bold, center),
                    append(''),
                    append(''),
                    next_row,
                    append(new(@P1, label(name)), bold, center),
                    append(new(@P2, label(name)), bold, center),
                    append(new(@P3, label(name)), bold, center),
                    append(new(@P4, label(name)), bold, center),
                    append(new(@P5, label(name)), bold, center),
                    next_row,
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    next_row,
                    append(''),
                    append(''),
                    append('Rozwiazanie', bold, center),
                    append(''),
                    append(''),
                    next_row,
                    append(new(@T1, label(name)), bold, center),
                    append(new(@T2, label(name)), bold, center),
                    append(new(@T3, label(name)), bold, center),
                    append(new(@T4, label(name)), bold, center),
                    append(new(@T5, label(name)), bold, center)
                  ]),
	send(new(D, dialog), below, Pict),
    send(D, append, button('Start', message(@prolog, start, E1, E2, E3, E4, E5, @T1, @T2, @T3, @T4, @T5))),
	send(Pict, open),
    send(@P1, append(E1)),
    send(@P2, append(E2)),
    send(@P3, append(E3)),
    send(@P4, append(E4)),
    send(@P5, append(E5)).



default():-
    write('Inactive').

start(E1, E2, E3, E4, E5, @T1, @T2, @T3, @T4, @T5):-
    solve([E1, E2, E3, E4, E5], X),
    last(X, Tail),
    nth0(0,Tail, R1),
    nth0(1,Tail, R2),
    nth0(2,Tail, R3),
    nth0(3,Tail, R4),
    nth0(4,Tail, R5),
    send(@T1, append, R1),
    send(@T2, append, R2),
    send(@T3, append, R3),
    send(@T4, append, R4),
    send(@T5, append, R5).


    last([_|Tail]):-
        last(Tail).

solve(B, P) :-
    path(B, [], P1),
    reverse(P1, P).

path(S, P, [S|P]) :- final_state(S).

path(S, P1, P) :-
    write('iterate'), nl,
    next_state(S, S1),
    write(S),
    not(member(S1, P1)),
    path(S1, [S1|P1], P).

next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- 
    transition([A,B], [A1,B1]).

next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- 
    transition([A,C], [A1,C1]).

next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- 
    transition([A,D], [A1,D1]).

next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- 
    transition([A,E], [A1,E1]).

next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- 
    transition([B,C], [B1,C1]).

next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- 
    transition([B,D], [B1,D1]).

next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- 
    transition([B,E], [B1,E1]).

next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- 
    transition([C,D], [C1,D1]).

next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- 
    transition([C,E], [C1,E1]).

next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- 
    transition([D,E], [D1,E1]).




transition([X,Y], [X1, Y1]) :-
    not_good(X, Y),
    X < 21,
    Y < 21,
    write(X),
    write('X +8'),
    nl,
    write(Y),
    write('Y +8'),
    nl,
    X1 is X+8,
    Y1 is Y+8,
    X1 =< 49,
    Y1 =< 49.



transition([X,Y], [X1, Y1]) :-
    not_good(X, Y),
    X > 25,
    Y > 25,
    write(X),
    write('X -13'), nl,
    write(Y),
    write('Y -13'),
    nl,
    X1 is X-13,
    Y1 is Y-13,
    X1 >= 0,
    Y1 >= 0.

transition([X,Y], [X1, Y1]) :-
    not_good(X, Y),
    X > 25; Y > 25,
    write(X),
    write('X +8'),
    nl,
    write(Y),
    write('Y +8'),
    nl,
    X1 is X+8,
    Y1 is Y+8,
    X1 =< 49,
    Y1 =< 49.


transition([X,Y], [X1, Y1]) :-
    not_good(X, Y),
    X < 21; Y < 21,
    write(X),
    write('X +8'),
    nl,
    write(Y),
    write('Y +8'),
    nl,
    X1 is X+8,
    Y1 is Y+8,
    X1 =< 49,
    Y1 =< 49.

not_good(A,B) :-
    not(good_position(A)),
    !,
    not(good_position(B)).

final_state([A,B,C,D,E]) :-
    good_position(A),
    good_position(B),
    good_position(C),
    good_position(D),
    good_position(E).

good_position(X) :-
    X >= 21,
    X =< 25.
    
