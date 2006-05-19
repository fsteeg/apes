%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A Prolog Expert System (APES)
% http://apes.sourceforge.net/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a DCG for describing the input format

% --------------------------------------------
% question
% -------------------------------------------- 

q(q(is,NP,NP2)) --> [is],np(NP),np(NP2),[?].

% --------------------------------------------
% rule
% -------------------------------------------- 

rule(rule(if,Condition,then,S)) --> [if],condition(Condition),[then],s(S),[.].

condition(condition(S)) --> s(S).
condition(condition(S,Link,C)) --> s(S),link(Link),condition(C).

s(s(N,V,AP)) --> n(N),v(V),ap(AP).

% --------------------------------------------
% words and phrases
% -------------------------------------------- 

n(N) --> [N].

ap(A) --> [A].
ap([A,B]) --> [A,B].

np(np(N)) --> n(N).
np(np(Det,N)) --> det(Det),n(N).
np(np(Det,AP)) --> det(Det),ap(AP).

v(has) --> [has].
v(isa) --> [is,a].
v(does) --> [does].

a(A) --> [A].

link(and) --> [and].
link(or) --> [or].

det(the) --> [the].
det(a) --> [a].
det(an) --> [an].