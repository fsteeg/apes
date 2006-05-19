/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A Prolog Expert System (APES)
% http://apes.sourceforge.net/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% 
%read user input 
%from clocksin & mellish
%"Programming in Prolog"
%%%%%%%%%%%%%%%%%%%%%%%%

like this:

?- read_in(S).
hello, world.
S=[hello,',',world,'.']

*/

read_in([Head|Tail]) :-
	get_char(Char),
	read_word(Char, Head, Char1),
	restsent(Head, Char1, Tail).

/* given a word and the char after it, 
read the rest */

% done when word is last word
restsent(Word, _, []) :-
	last_word(Word),
	!.

restsent(_,Char,[Head|Tail]) :- 
	read_word(Char, Head, Char1),
	restsent(Head, Char1, Tail).

/* 
read a single word, given an initial char,
and remembering which character came after 
the word 

- first param is for new read character
- second param for the atom to be 
constructed from the word
- third param is first character after 
the word
*/

/* 
satisfied if char is not a 'single_char'
// go on reading
*/
read_word(Char, Char, Char1) :-
	single_char(Char),
	!,
	get_char(Char1).

% found word
read_word(Char,Word,Char2) :-
	in_word(Char, NewChar),
	!,
	get_char(Char1),
	rest_word(Char1,Chars,Char2),
	atom_chars(Word,[NewChar|Chars]).

read_word(_, Word, Char2) :-
	get_char(Char1),
	read_word(Char1, Word, Char2).

/**/

rest_word(Char,[NewChar|Chars],Char2):-
	in_word(Char, NewChar),
	!,
	get_char(Char1), 
	rest_word(Char1,Chars,Char2).

rest_word(Char,[],Char).

/* character that can appear in a word:
letter,digit,special */

in_word(Char,Char) :- letter(Char,_). % ab ...
in_word(Char,Letter) :- letter(Letter,Char). % AB ...
in_word(Char,Char) :- digit(Char). %12 ...
in_word(Char,Char) :- special_char(Char). % .

/* special, part of words */

special_char('-').
special_char('"').

/* words on their own */

single_char(',').
single_char('.').
single_char(';').
single_char(':').
single_char('?').
single_char('!').
single_char('(').
single_char(')').


/* letters */

letter(a,'A').
letter(b,'B').
letter(c,'C').
letter(d,'D').
letter(e,'E').
letter(f,'F').
letter(g,'G').
letter(h,'H').
letter(i,'I').
letter(j,'J').
letter(k,'K').
letter(l,'L').
letter(m,'M').
letter(n,'N').
letter(o,'O').
letter(p,'P').
letter(q,'Q').
letter(r,'R').
letter(s,'S').
letter(t,'T').
letter(u,'U').
letter(v,'V').
letter(w,'W').
letter(x,'X').
letter(y,'Y').
letter(z,'Z').

/* digits */

digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

/* these words terminate sentences */

last_word('.').
last_word('!').
last_word('?').