:- module(run, [run/0]).
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(lists)).
run:-
	initial().

initial():-
    E1 = 17,
    E2 = 26,
    E3 = 20,
    E4 = 19,
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
    next_state(S, S1),
    not(member(S1, P1)),
    write(S),
    nl,
    path(S1, [S1|P1], P).


next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- not_good(A,B), both_up_or_down(A,B), transition([A,B], [A1,B1]).
next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- not_good(A,C), both_up_or_down(A,C), transition([A,C], [A1,C1]).
next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- not_good(A,D), both_up_or_down(A,D), transition([A,D], [A1,D1]).
next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- not_good(A,E), both_up_or_down(A,E), transition([A,E], [A1,E1]).
next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- not_good(B,C), both_up_or_down(B,C), transition([B,C], [B1,C1]).
next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- not_good(B,D), both_up_or_down(B,D), transition([B,D], [B1,D1]).
next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- not_good(B,E), both_up_or_down(B,E), transition([B,E], [B1,E1]).
next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- not_good(C,D), both_up_or_down(C,D), transition([C,D], [C1,D1]).
next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- not_good(C,E), both_up_or_down(C,E), transition([C,E], [C1,E1]).
next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- not_good(D,E), both_up_or_down(D,E), transition([D,E], [D1,E1]).

next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- A=<20, B<25, transition([A,B], [A1,B1]).
next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- A=<20, C<25, transition([A,C], [A1,C1]).
next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- A=<20, D<25, transition([A,D], [A1,D1]).
next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- A=<20, E<25, transition([A,E], [A1,E1]).

next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- B=<20, A<25, transition([A,B], [A1,B1]).
next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- B=<20, C<25, transition([B,C], [B1,C1]).
next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- B=<20, D<25, transition([B,D], [B1,D1]).
next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- B=<20, E<25, transition([B,E], [B1,E1]).

next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- C=<20, B < 25, transition([B,C], [B1,C1]).
next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- C=<20, A < 25, transition([A,C], [A1,C1]).
next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- C=<20, D < 25, transition([C,D], [C1,D1]).
next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- C=<20, E < 25, transition([C,E], [C1,E1]).

next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- D=<20, B<25, transition([B,D], [B1,D1]).
next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- D=<20, C<25, transition([C,D], [C1,D1]).
next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- D=<20, A<25, transition([A,D], [A1,D1]).
next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- D=<20, E<25, transition([D,E], [D1,E1]).

next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- E=<20, B<25, transition([B,E], [B1,E1]).
next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- E=<20, C<25, transition([C,E], [C1,E1]).
next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- E=<20, D<25, transition([D,E], [D1,E1]).
next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- E=<20, A<25, transition([A,E], [A1,E1]).



next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- A>25, B>21, transition([A,B], [A1,B1]).
next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- A>25, C>21, transition([A,C], [A1,C1]).
next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- A>25, D>21, transition([A,D], [A1,D1]).
next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- A>25, E>21, transition([A,E], [A1,E1]).

next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- B>25, A>21, transition([A,B], [A1,B1]).
next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- B>25, C>21, transition([B,C], [B1,C1]).
next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- B>25, D>21, transition([B,D], [B1,D1]).
next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- B>25, E>21, transition([B,E], [B1,E1]).

next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- C>25, B>21, transition([B,C], [B1,C1]).
next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- C>25, A>21, transition([A,C], [A1,C1]).
next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- C>25, D>21, transition([C,D], [C1,D1]).
next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- C>25, E>21, transition([C,E], [C1,E1]).

next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- D>25, B>21, transition([B,D], [B1,D1]).
next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- D>25, C>21, transition([C,D], [C1,D1]).
next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- D>25, A>21, transition([A,D], [A1,D1]).
next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- D>25, E>21, transition([D,E], [D1,E1]).

next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- E>25, B>21, transition([B,E], [B1,E1]).
next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- E>25, C>21, transition([C,E], [C1,E1]).
next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- E>25, D>21, transition([D,E], [D1,E1]).
next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- E>25, A>21, transition([A,E], [A1,E1]).

    
transition([X,Y], [X1, Y1]) :-
	X < 21,	
	X1 is X+8,
    Y1 is Y+8,
    write('X: '),
    write(X),
    write(' +8'),
    nl,
    write('Y: '),
    write(Y),
    write(' +8'),
    nl.

transition([X,Y], [X1, Y1]) :-
	X >= 21,
	X1 is X-13,
    Y1 is Y-13,
    write('X: '),
    write(X),
    write(' -13'),
    nl,
    write('Y: '),
    write(Y),
    write(' -13'),
    nl.

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

both_up_or_down(X,Y):-
X < 21, Y < 21.

both_up_or_down(X,Y):-
X > 25, Y > 25.