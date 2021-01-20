:- module(run, [run/0]).
:- use_module(library(pce)).
:- use_module(library(tabular)).


run:-
	initial().

initial():-
	new(Pict, window('Elevators logic',size(950,300))),
    send(Pict, display, new(T, tabular)),
        send(T, border, 0),
        send(T, cell_spacing, -1),
        send(T, rules, all),
        send_list(T,
                  [ append('pozycja windy'),
                    append('pozycja windy'),
                    append('pozycja windy'),
                    append('pozycja windy'),
                    append('pozycja windy'),
                    next_row,
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg'))
                  ]),
	send(new(D, dialog), below, Pict),
    send(D, append, button('Krok', message(@prolog, step))),
	send(new(E, dialog), below, Pict),
    send(E, append, label(reporter)),
	send(Pict, open).

% Step 1: 1 & 3 up
% Step 2: 4 & 5 up
% Step 3: 4 & 5 down
% Step 4: 2 & 5 up
% Step 5: 2 & 5 down
% Step 6: 3 & 5 up
% Step 7: 3 & 5 down
% Step 8: 4 & 5 up

setFloor(X, X1):-
    X is X1.

upFloor(X, X1):-
    X1 is X+8.

downFloor(X, X1):-
    X1 is X-13.

step1(X1, X3, R1, R2):-
    upFloor(X1, R1),
    upFloor(X3, R2).

start(X1, X2, X3, X4, X5):-
    setFloor(X1, 17),
    setFloor(X2, 26),
    setFloor(X3, 20),
    setFloor(X4, 19),
    setFloor(X5, 31).