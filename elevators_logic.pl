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
    send(D, append, button('Reset', message(@prolog, clear))),
	send(new(E, dialog), below, Pict),
    send(E, append, label(reporter)),
	send(Pict, open).

step():-
    send(reporter, 'Step').

clear() :-
    write('Clear program').