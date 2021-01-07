:- module(start, [start/0]).
:- use_module(library(pce)).

start:-
	renderView().

renderView():-
	new(Pict, window('Elevator logic', size(1000, 500))),
	send(Pict, open),
	% elevator 1
	new(Elevator1, figure),
	send(Elevator1, display, bitmap('images/elevator1.jpg')),
	send(Pict, display, Elevator1, point(0, 141)),
	% elevator 2
	new(Elevator2, figure),
	send(Elevator2, display, bitmap('images/elevator2.jpg')),
	send(Pict, display, Elevator2, point(200, 141)),
	% elevator 3
	new(Elevator3, figure),
	send(Elevator3, display, bitmap('images/elevator3.jpg')),
	send(Pict, display, Elevator3, point(400, 141)),
	% elevator 4
	new(Elevator4, figure),
	send(Elevator4, display, bitmap('images/elevator4.jpg')),
	send(Pict, display, Elevator4, point(600, 141)),
	% elevator 5
	new(Elevator5, figure),
	send(Elevator5, display, bitmap('images/elevator5.jpg')),
	send(Pict, display, Elevator5, point(800, 141)).