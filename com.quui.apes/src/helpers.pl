%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% An Expert System in Prolog 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --------------------------------------------
% helpers zur eingabe von if-then-regeln
% 
% eine regel hat nur ein objekt, es wird 
% immer durchgereicht (das 2. Argument von
% processStatement)
% -------------------------------------------- 

% mehrere statements, zerlegen
processStatement(Conditions,N):-
    link(_,[Link],[]),
    Conditions = condition(S,Link,Rest),
    assignN(S,N),
    append('knowledge/knowledge.pl'),
    writeStatement(S,N), nl,tab(10),
    write(Link), write(' '), nl,tab(5),
    processStatement(Rest,N).    
    
% eine condition aus einem satz
processStatement(Condition,N):-
    Condition = condition(S),
    assignN(S,N),
    append('knowledge/knowledge.pl'),
    writeStatement(S,N).

% einem satz
processStatement(S,N):-
    append('knowledge/knowledge.pl'),
    writeStatement(S,N).

% if the rule has no object yet take the n from the first 
% sentence and define that as the object, otherwise leave
% it alone.
assignN(From,Old):-
    \+ atom(Old),
    From = s(Old,_,_).
assignN(_,_).

% ein statement a la 'X isa good flier'
writeStatement(S,N):-
    S = s(_,V,AP),
    ensureList(AP,Neu),
    atom_concat('Var_',N,N1),    append('knowledge/knowledge.pl'),
    append([N1],[V],VarV), 
    writeList(VarV),
    write(' '), % ... 'good flier'
    write('\''), writeList(Neu), write('\''), 
    write(' '),!.

% ------------------------------------
% helpers zur eingabe von fakten
% ------------------------------------

% zur ausgabe der bestehenden regeln
readAllClauses(X):-
    at_end_of_stream(X),
    close(X),nl.
readAllClauses(X):-
    read_clause(X,Result),
    write(Result),nl,
    readAllClauses(X).

% pro neue gruppe eine datei anlegen
createFileWithDynDecl(Name):-
    % create new file for group, incl dynamic declaration:
    atom_concat('knowledge/groups/',Name,X),
    atom_concat(X,'.pl',File),
    %write('appending to: '),write(File),nl,
    append(File),
    atom_concat(':- dynamic(',Name,Buff),
    atom_concat(Buff,'/1).',DynDec),
    write(DynDec),
    told.

addItem(Item,Group):-
    % write to file with name of group in folder 'groups':
    atom_concat('knowledge/groups/',Group,X),
    atom_concat(X,'.pl',File),
    write('appending to: '),write(File),nl,
    append(File),
    Predicate =.. [Group,Item],
    assert(Predicate),
    nl, write(Predicate),
    write('.'),nl,
    told,
    write('ok, added item: '), write(Item),
    write(' to group: '), write(Group), nl.  

% ------------------------------------
% helpers zum fragen
% ------------------------------------

% zum fragen is the moon a planet ---- > moon isa planet
removeDet(X,Result):-
    X=[Result]. 
removeDet(X,Result):-
    X=[_,Result].

ensureAtom(Test,Result):-
    atom(Test),
    Result = Test.
ensureAtom(Test,Result):-
    listToAtom(Test,_,Result).

% [green,greasy,hair] ---- > 'green greasy hair'
listToAtom([],_,_).
% last element:
listToAtom([H|[]],Atom,Result):-
    atom(Atom),
    atom_concat(Atom,' ',AtomB),
    atom_concat(AtomB,H,Atom2),
    Result = Atom2.
% in between:
listToAtom([H|T],Atom,Result):-
    atom(Atom),
    atom_concat(Atom,' ',AtomB),
    atom_concat(AtomB,H,Atom2),
    listToAtom(T,Atom2,Result).
% first element:
listToAtom([H|T],_,Result):-
    H = Atom,
    listToAtom(T,Atom,Result).

% bevor es an writeList uebergeben wird
ensureList(X,Neu):-
    is_list(X),
    Neu = X.
ensureList(X,Neu):-
    \+ is_list(X),
    Neu = [X].
    
% schreibt zwischen alle elemente ein leerzeichen ausser nach dem letzten 
writeList([]).
writeList([H|T]):-
    write(H), 
    T \= [],
    write(' '),
    writeList(T).
writeList([_|T]):-
    T = [],
    writeList(T).




