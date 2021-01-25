:- module(run, [run/0]).
:- use_module(library(pce)).
:- use_module(library(tabular)).


run:-
	initial().

initial():-
	new(Pict, window('Elevators logic',size(950,350))),
    send(Pict, display, new(T, tabular)),
        send(T, border, 0),
        send(T, cell_spacing, -1),
        send(T, rules, all),
        send_list(T,
                  [ append(''),
                    append(''),
                    append('+ 8'),
                    append(''),
                    append(''),
                    next_row,
                    append('pozycja windy'),
                    append('pozycja windy'),
                    append('pozycja windy'),
                    append('pozycja windy'),
                    append('pozycja windy'),
                    next_row,
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    append(bitmap('elevator.jpg')),
                    next_row,
                    append(''),
                    append(''),
                    append('- 13'),
                    append(''),
                    append('')
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

% 17
% 26
% 20
% 19
% 31