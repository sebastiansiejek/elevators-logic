:- module(start, [start/0]).
:- use_module(library(pce)).

start:-
	renderView().

renderView():-
	new(Pict, window('Elevator logic', size(800, 320))),
	send(Pict, open).