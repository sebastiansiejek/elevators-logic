% :- module(run, [run/0]).
% :- use_module(library(pce)).
% :- use_module(library(tabular)).


% run:-
% 	initial().

% initial():-
% 	new(Pict, window('Elevators logic',size(950,350))),
%     send(Pict, display, new(T, tabular)),
%         send(T, border, 0),
%         send(T, cell_spacing, -1),
%         send(T, rules, all),
%         send_list(T,
%                   [ append(''),
%                     append(''),
%                     append(button('+8'), bold, center),
%                     append(''),
%                     append(''),
%                     next_row,
%                     append('17', bold, center),
%                     append('26', bold, center),
%                     append('20', bold, center),
%                     append('19', bold, center),
%                     append('31', bold, center),
%                     next_row,
%                     append(bitmap('elevator.jpg')),
%                     append(bitmap('elevator.jpg')),
%                     append(bitmap('elevator.jpg')),
%                     append(bitmap('elevator.jpg')),
%                     append(bitmap('elevator.jpg')),
%                     next_row,
%                     append(''),
%                     append(''),
%                     append(button('- 13'), bold, center),
%                     append(''),
%                     append('')
%                   ]),
% 	send(new(D, dialog), below, Pict),
%     send(D, append, button('Start', message(@prolog, step))),
% 	send(new(E, dialog), below, Pict),
%     send(E, append, label(reporter)),
% 	send(Pict, open).

% Step 1: 1 & 3 up
% Step 2: 4 & 5 up
% Step 3: 4 & 5 down
% Step 4: 2 & 5 up
% Step 5: 2 & 5 down
% Step 6: 3 & 5 up
% Step 7: 3 & 5 down
% Step 8: 4 & 5 up

% solve([21, 22, 23, 24, 25], X).
% solve([17, 26, 20, 19, 31], X).

solve(B, P) :-
    path(B, [], P1),
    reverse(P1, P).

path(S, P, [S|P]) :- final_state(S).
path(S, P1, P) :-
    next_state(S, S1),
    not(member(S1, P1)),
    path(S1, [S1|P1], P).

next_state([A,B,C,D,E], [A1,B1,C,D,E]) :- transition([A,B], [A1,B1]).
next_state([A,B,C,D,E], [A1,B,C1,D,E]) :- transition([A,C], [A1,C1]).
next_state([A,B,C,D,E], [A1,B,C,D1,E]) :- transition([A,D], [A1,D1]).
next_state([A,B,C,D,E], [A1,B,C,D,E1]) :- transition([A,E], [A1,E1]).
next_state([A,B,C,D,E], [A,B1,C1,D,E]) :- transition([B,C], [B1,C1]).
next_state([A,B,C,D,E], [A,B1,C,D1,E]) :- transition([B,D], [B1,D1]).
next_state([A,B,C,D,E], [A,B1,C,D,E1]) :- transition([B,E], [B1,E1]).
next_state([A,B,C,D,E], [A,B,C1,D1,E]) :- transition([C,D], [C1,D1]).
next_state([A,B,C,D,E], [A,B,C1,D,E1]) :- transition([C,E], [C1,E1]).
next_state([A,B,C,D,E], [A,B,C,D1,E1]) :- transition([D,E], [D1,E1]).

transition([X,Y], [X1, Y1]) :-
    write('+8'),
    X1 is X+8,
    Y1 is Y+8,
    X1 =< 49,
    Y1 =< 49.

transition([X,Y], [X1, Y1]) :-
    write('-13'),
    X1 is X-13,
    Y1 is Y-13,
    X1 >= 0,
    Y1 >= 0.

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
