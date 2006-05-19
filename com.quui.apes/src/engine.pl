%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A Prolog Expert System (APES)
% http://apes.sourceforge.net/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% From Bratko, Prolog Progr. for AI, 2000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(100, xfx, [has, isa, does]).

:- op(900,fx,not).
:- op(900, xfx, ::).

:- op(800, xfx, was).
:- op(870, fx, if).
:- op(880, xfx, then).
:- op(550, xfy, or).
:- op(540, xfy, and).
:- op(300, fx, 'derived by').
:- op(600, xfx, from).
:- op(600, xfx, by).

explore(Goal, _Trace, Goal is true was 'found as a fact') :-
    fact :: Goal.
    
% assume only one rule about each type of goal

explore(Goal,Trace,Goal is TruthValue was 'derived by' Rule from Answer):-
    % Rule relevant to Goal:
    Rule :: if Condition then Goal,
    explore(Condition,[Goal by Rule | Trace],Answer),
    truth(Answer, TruthValue).
    
explore(Goal1 and Goal2,Trace, Answer):- 
    !,
    explore(Goal1,Trace,Answer1),
    continue(Answer1,Goal1 and Goal2,Trace,Answer).

explore(Goal1 or Goal2,Trace,Answer):-
    % positive answer to Goal 1:
    exploreyes(Goal1,Trace,Answer)
    ;
    % positive answer to Goal 2:
    exploreyes(Goal2,Trace,Answer).
    
explore(Goal1 or Goal2,Trace,Answer1 and Answer2) :-
    !,
    not (exploreyes(Goal1,Trace,_)),
    % no positive answer:
    not (exploreyes(Goal2,Trace,_)),
    % Answer 1 must be negative:
    explore(Goal1,Trace,Answer1),
    % Answer 2 must be negative:
    explore(Goal2,Trace,Answer2).
    
explore(Goal,Trace,Goal is Answer was told) :-
    % user-supplied answer:
    useranswer(Goal,Trace,Answer).
    
exploreyes(Goal,Trace,Answer):-
    explore(Goal,Trace,Answer),
    positive(Answer).

continue(Answer1,_Goal1 and Goal2,Trace,Answer):-
    positive(Answer1),
    explore(Goal2,Trace,Answer2),
    (   positive(Answer2),
        Answer = Answer1 and Answer2
        ;
        negative(Answer2),
        Answer = Answer2
    ).

continue(Answer1,_Goal1 and _Goal2,_,Answer1):-
    negative(Answer1).

truth(_Question is TruthValue was _Found, TruthValue) :- !.

truth(Answer1 and Answer2, TruthValue) :-
    truth(Answer1,true),
    truth(Answer2,true),
    !,
    TruthValue = true
    ;
    TruthValue = false.

positive(Answer):-
    truth(Answer,true).
negative(Answer):-
    truth(Answer,false).
    
% .................. 
% uses read and write
% not fully declarative

getreply(Reply):-
    read(Answer),
    means(Answer,Reply),
    !
    ;
    nl, write('Answer unknown, try again please'),nl,
    getreply(Reply).
    
means(yes,yes).
means(y,yes).
means(no,no).
means(n,no).
means(why,why).
means(w,why).

% ....................

% ....................
% procedure useranswer

% generates, through backtracking, user-supplied-solution to goal
% trace is a chain of ancestor goals and rules

useranswer(Goal, Trace,Answer):-
    % may be asked of the user:
    askable(Goal,_),
    % variables in goal renamed:
    freshcopy(Goal,Copy),
    useranswer(Goal, Copy, Trace, Answer, 1).
    
% do not ask about uninstantiated goal:
useranswer(Goal,_,_,_,N):-
    % repeated question?
    N>1,
    % no variables in goal:
    instantiated(Goal),
    % do not ask again:
    !, fail.

% is goal implied true or false for all instantiations?
useranswer(Goal,Copy,_,Answer,_):-
    wastold(Copy,Answer,_),
    % answer to goal implied:
    instance_of(Copy,Goal),
    !.

% retrieve known solutions, indexed from n on, for goal:
useranswer(Goal,_,_,true,N):-
    wastold(Goal,true,M),
    M >= N.
    
% has everything been said about goal?
useranswer(Goal,Copy,_,_Answer,_):-
    end_answers(Copy),
    % everything was already said about goal:
    instance_of(Copy,Goal),
    !,fail.

% ask the user for (more) solutions:
useranswer(Goal,_,Trace,Answer,N):-
    askuser(Goal,Trace,Answer,N).
askuser(Goal,Trace,Answer,N):-
    askable(Goal,ExternFormat),
    % get question format:
    format(Goal,ExternFormat,Question,[],Variables),
    ask(Goal,Question,Variables,Trace,Answer,N).
ask(Goal,Question,Variables,Trace,Answer,N):-
    nl,
    (   Variables = [], 
        !,
        % introduce question:
        write('Is it true: ')
        ;
        % introduce question
        write('Any (more) solution to: ')
    ),
    write(Question),
    write('? '),
    % reply = yes/no/why
    getreply(Reply),
    !,
    process(Reply, Goal, Question, Variables, Trace, Answer, N).

process(why,Goal,Question,Variables,Trace,Answer,N):-
    showtrace(Trace),
    ask(Goal,Question,Variables,Trace,Answer,N).
    
process(yes, Goal,_,Variables,Trace,true,_N):-
    % get new free index for wastold
    nextindex(Next),
    Next1 is Next + 1,
    (   askvars(Variables),
        % record solution:
        assertz(wastold(Goal,true,Next))
        ;
        % copy of goal
        freshcopy(Goal,Copy),
        % more answers?
        useranswer(Goal,Copy,Trace,_Answer,Next1)
    ).

process(no, Goal, _, _, _, false, _N):-
    freshcopy(Goal,Copy),
    % 'no' menas: no more solutions:
    wastold(Copy,true,_),
    !,
    % mark end of answers
    assertz(end_answers(Goal)),
    fail
    ;
    % next free index for 'wastold'
    nextindex(Next),
    % no means no solutions
    assertz(wastold(Goal,false,Next)).
    
format(Var,Name,Name,Vars,[Var/Name | Vars]):-
    var(Var),
    !.
format(Atom,Name,Atom,Vars,Vars):-
    atomic(Atom),
    atomic(Name).
    
format(Goal,Form,Question,Vars0,Vars):-
    Goal=..[Functor|Args1],
    Form=..[Functor|Forms],
    formatall(Args1,Forms,Args2,Vars0,Vars),
    Question=..[Functor|Args2].

formatall([],[],[],Vars,Vars).

formatall([X|XL],[F|FL],[Q|QL],Vars0,Vars) :-
    formatall(XL,FL,QL,Vars0,Vars1),
    format(X,F,Q,Vars1,Vars).

askvars([]).

askvars([Variable/Name | Variables]):-
    nl, write(Name), write(' ='),
    read(Variable),
    askvars(Variables).

% done:
showtrace([]):-
    nl, write('This was your question'),nl.
    
showtrace([Goal by Rule | Trace]) :-
    nl, write('To investigate, by '),
    write(Rule), write(','),
    write(Goal),
    showtrace(Trace).

instantiated(Term):-
    % no variables in term
    numbervars(Term,0,0).

% instance of(T1,T2): instance of t1 is t2, that is, t1 is more general
% or equally general as t2

% instance of term is term1
instance_of(Term,Term1):-
    % refresh variables
    freshcopy(Term1,Term2),
    numbervars(Term2, 0,_),
    !,
    % this succeeds if term1 is instance of term
    Term = Term2.

% copy with fresh variables    
freshcopy(Term,FreshTerm):-
            asserta(copy(Term)),
            retract(copy(FreshTerm)),
            !.

% next free index for wastold            
nextindex(Next) :-
    retract(lastindex(Last)),
    !,
    Next is Last + 1,
    assert(lastindex(Next)).
    
% initialize dynamic procedures lastindex/1,wastold/3,end_answers/1
:-  assertz(lastindex(0)),
    assertz(wastold(dummy,false,0)),
    assertz(end_answers(dummy)).
    
% ..............................

% display the conclusion of a consultation and how-explanation

present(Answer):-
    nl, showconclusion(Answer),
    nl, write('Would you like to see how? '),
    getreply(Reply),
    (   Reply = yes,
        !,
        %show solution tree
        show(Answer)
        ;
        true
    ).

showconclusion(Answer1 and Answer2):-
    !,
    showconclusion(Answer1), write('and '),
    showconclusion(Answer2).

showconclusion(Conclusion was _Found):-
    write(Conclusion).
    
% 'show' displays a complete solution tree

show(Solution):-
    % indent by 0
    nl, show(Solution,0),
    !.
show(Answer1 and Answer2,H):-
    !,
    % indent by H
    show(Answer1,H),
    tab(H),write(and),nl,
    show(Answer2,H).
show(Answer was Found,H):-
    % show conclusion
    tab(H), writeans(Answer),
    nl, tab(H),
    write('was '),
    % show evidence
    show1(Found,H).

show1(Derived from Answer,H):-
    !,
    % show rule name
    write(Derived), write(' from '),
    nl,H1 is H + 4,
    % show antecedent
    show(Answer,H1).

% found = told or 'found as fact'
show1(Found,_):-
    write(Found),nl.

writeans(Goal is true) :- 
    !,
    % omit 'is true' on output
    write(Goal).

% this is negative answer
writeans(Answer):-
    write(Answer).
    

% .......................
% top-level driving procedure

expert:-
    % input users question:
    getquestion(Question),
    % try to find positive answer
    (   answeryes(Question)
        ;
        % if no positive answer then find negative
        answerno(Question)
    ).
    
% look for positive answers to question
answeryes(Question):-
    % no positive answer yet
    markstatus(negative),
    % trace is empty
    explore(Question,[],Answer),
    % look for positive answers
    positive(Answer),
    % positive answer found
    markstatus(positive),
    present(Answer), nl,
    write('More solutions? '),
    getreply(Reply),
    % otherwise backtrack to 'explore'
    Reply = no.
    
% look for negative answer to question
answerno(Question):-
    % has there been no positive answer?
    retract(no_positive_answer_yet),
    !,
    explore(Question,[],Answer),
    negative(Answer),
    present(Answer),nl,
    write('More negative solutions? '),
    getreply(Reply),
    Reply = no.

markstatus(negative):-
    assert(no_positive_answer_yet).

markstatus(positive):-
    retract(no_positive_answer_yet),
    !
    ;
    true.
    
getquestion(Question):-
    nl,write('Question, please'),nl,
    read(Question).
    

