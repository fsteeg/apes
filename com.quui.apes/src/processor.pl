%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% An Expert System in Prolog 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this isnt working
process([stop,'.']):-
    !,fail.
% this isnt working
process([help,'.']):-
    open('../README',read,X),
    readAllClauses(X).

% --------------------------------------------
% eingabe von if-then-regeln
% --------------------------------------------      

process(Rule):-
    % parse input
    rule(Structure,Rule,[]),
    Structure=rule(if,Condition,then,Consequence),
    append('knowledge/knowledge.pl'),
    write('% new rule added from shell:'), nl,
    write('rule::if'),nl,tab(5),
    processStatement(Condition,N),nl,
    write('then'),nl,tab(5),
    processStatement(Consequence,N),
    write('.'),nl, 
    told,
    consult('knowledge/knowledge.pl'),
    write('ok, added rule: '),writeList(Rule),nl.

% --------------------------------------------
% fragen
% -------------------------------------------- 

process(Q) :-
    q(Structure,Q,[]),
    Structure = q(is,NP1,NP2),
    NP1 =.. [_|Object],
    NP2 =.. [_|Something],
    removeDet(Object,O),
    removeDet(Something,S0),
    ensureAtom(S0,S),
    %write('name: '), write(Object), nl,
    (   
        % ab in bratko
        answeryes(O isa S)
        ;
        answerno(O isa S)
    ).

process([what,is,Object,'?']) :-
    %write('name: '), write(Object), nl,
    (   
        % ab in bratko
        answeryes(Object isa X)
        ;
        answerno(Object isa X)
    ).  

% --------------------------------------------
% eingabe von fakten
% -------------------------------------------- 

process([create,group,Name,'.']) :-
    append('knowledge/knowledge.pl'),
    write('% new group added from shell: '),nl,
    atom_concat('fact::X isa ',Name,B),
    atom_concat(B, ' :-', Head),
    write(Head), nl,
    atom_concat(Name,'(X).',Body),
    write(Body),nl,
    told,
    consult('knowledge/knowledge.pl'),
    createFileWithDynDecl(Name),
    dynamic(Name/1),
    write('ok, added new group: '), write(Name), nl.   

process([add,Item,to,Group,'.']) :-
    addItem(Item,Group).
process([Item,is,a,Group,'.']):-
    addItem(Item,Group). 
    
process([show, knowledge,'.']):-
    open('knowledge/knowledge.pl',read,X),
    readAllClauses(X).

process([show, group, X,'.']):-
    atom_concat('knowledge/groups/',X,B),
    atom_concat(B,'.pl',File),
    exists_file(File),
    open(File,read,Z),
    readAllClauses(Z).

process(_):-
	write('> >  can\'t comprehend  < <'),
	nl,!.