:- module(run, [run/0]).
:- use_module(library(pce)).
:- use_module(library(tabular)).

run:-
	initial().

initial():-
	new(Pict, window('Elevators logic',size(950,300))),
	send(new(A, dialog), above, Pict),
    send(A, append, button('+ 8', message(@prolog, up))),

    send(Pict, display, new(T, tabular)),
        send(T, border, 0),
        send(T, cell_spacing, -1),
        send(T, rules, all),
        send_list(T,
                  [ append(button('Wybierz', message(@prolog, choose))),
                    append(button('Wybierz', message(@prolog, choose))),
                    append(button('Wybierz', message(@prolog, choose))),
                    append(button('Wybierz', message(@prolog, choose))),
                    append(button('Wybierz', message(@prolog, choose))),
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
                    append(bitmap('elevator.jpg'))
                  ]),
	send(new(C, dialog), below, Pict),
    send(C, append, button('- 13', message(@prolog, down))),
	send(new(D, dialog), below, Pict),
    send(D, append, button('Reset program', message(@prolog, clear))),
	send(new(E, dialog), below, Pict),
    send(E, append, label(reporter)),
	send(Pict, open).

choose():-
    write('Wybrano winde').


up():-
    send(reporter,  'Up 8').


down():-
    write('Down 13').
    


clear() :-
    write('Clear program').