%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A Prolog Expert System (APES)
% http://apes.sourceforge.net/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('engine.pl').
:- ensure_loaded('tokenizer.pl').
:- ensure_loaded('parser.pl').
:- ensure_loaded('knowledge/knowledge.pl').
:- ensure_loaded('knowledge/groups/*.pl').
:- ensure_loaded('helpers.pl').
:- ensure_loaded('processor.pl').

start:-
    %dynamic((::)/2),
   	write('_____________________________________________________'),
	nl,nl,
	loop_read_in(_).

loop_read_in(X):-
    % from tokenizer.pl (clocksin & mellish, "programming in prolog"):
	read_in(X),
	process(X),
	loop_read_in(_).

loop_read_in(X):-
	read_in(X),	
	not(process(X)),
	!,fail.

