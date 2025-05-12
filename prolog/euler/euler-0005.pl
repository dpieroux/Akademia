:- use_module('lib/arithm.pl').

% euler(N, Res) 
%   Res is the smallest positive number that is evenly divisible by all of the
%   numbers from 1 to N.

euler(N, Res) :- euler(N, [], Res).

euler(N, AccPrimeFactors, Res) :-
    N>1,
    prime_factors(N, PrimeFactorsN),
    merge_factor(PrimeFactorsN, AccPrimeFactors, AccPrimeFactors1),
    N1 is N-1,
    euler(N1, AccPrimeFactors1, Res).

euler(1, AccPrimeFactors, Res) :- unfactor(AccPrimeFactors, Res).

merge_factor([(P1, E1) | PF1], [(P2, E2) | PF2], [(P1, E1) | PF3]) :-
    P1 < P2,
    merge_factor(PF1, [(P2, E2) | PF2], PF3).

merge_factor([(P, E1) | PF1], [(P, E2) | PF2], [(P, E) | PF3]) :-
    E is max(E1, E2),
    merge_factor(PF1, PF2, PF3).

merge_factor([(P1, E1) | PF1], [(P2, E2) | PF2], [(P2, E2) | PF3]) :-
    P1 > P2,
    merge_factor([(P1, E1) | PF1], PF2, PF3).

merge_factor([], PF2, PF2) :- PF2 = [_|_]. 
merge_factor(PF1, [], PF1).

%-------------------------------------------------------------------------------

test :- euler(10, 2520), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(20, X), write('Result: '), writeln(X).