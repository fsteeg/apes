%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A Prolog Expert System (APES)
% http://apes.sourceforge.net/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for use with engine from 
% Bratko, Prolog Progr. for AI, 2000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% define operators, making this an internal, Prolog-based domain-specific language (DSL)
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

askable(_ has _, 'Var_animal' has 'Something').
askable(_ does _, 'Var_animal' does 'Something').

% Note that this is only a small sample knowledge base, 
% the actualy KB is to be build by the user for the particular domain.

% each rule is represented by one predicate, using operators defined above:
rule0::if   Var_animal has 'a nervous system'
            or
            Var_animal has blood
        then
            Var_animal isa animal.
            
rule1::if   Var_animal isa animal and 
            Var_animal has hair
            or
            Var_animal does 'give milk'
        then
            Var_animal isa mammal.

rule2::if
            Var_animal isa animal and 
            Var_animal has feathers
            or
            Var_animal does fly and
            Var_animal does 'lay eggs'
        then
            Var_animal isa bird.
            
rule3::if
            Var_animal isa mammal and
            (Var_animal does 'eat meat'
                or
                Var_animal has 'pointed teeth' and
                Var_animal has claws and
                Var_animal has 'forward pointing eyes')
        then
            Var_animal isa carnivore.

rule4::if   
            Var_animal isa carnivore and
            Var_animal has 'tawny colour' and
            Var_animal has 'dark spots'
        then
            Var_animal isa cheetah.
            
rule5::if   
            Var_animal isa carnivore and
            Var_animal has 'tawny colour' and
            Var_animal has 'black stripes'
        then
            Var_animal isa tiger.
            
rule6::if
            Var_animal isa animal and
            Var_animal isa bird and
            Var_animal does 'not fly' and
            Var_animal does swim
        then
            Var_animal isa pinguin.
            
rule7::if
            Var_animal isa bird and
            Var_animal does 'fly good'
        then
            Var_animal isa albatross.

% new group added from shell: 
fact::X isa pet :-
pet(X).
% new rule added from shell:
rule::if
     Var_animal isa 'mammal' 
          and 
     Var_animal has 'two legs' 
then
     Var_animal isa 'humanoid' .
% new rule added from shell:
rule::if
     Var_animal isa 'humanoid' 
          and 
     Var_animal has 'language' 
then
     Var_animal isa 'human' .
% new rule added from shell:
rule::if
     Var_animal isa 'humanoid' 
          and 
     Var_animal has 'tools' 
then
     Var_animal isa 'erectus' .     
% new rule added from shell:
rule::if
     Var_animal has 'scales' 
          and 
     Var_animal has 'cold-blood' 
then
     Var_animal isa 'reptile' .
% new group added from shell: 
fact::X isa feline :-
feline(X).
