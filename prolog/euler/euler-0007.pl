:- use_module("lib/prime_gen.pl").

% euler(N, Res) Res is the N'th Prime.
euler(N, Res) :- 
    prime_gen_new(PrimeGen),
    nth_prime(N, PrimeGen, Res).

nth_prime(N, PrimeGen, Res) :-
    N>1, 
    prime_gen_next(PrimeGen, PrimeGen1),
    N1 is N-1,
    nth_prime(N1, PrimeGen1, Res).

nth_prime(1, PrimeGen, Res) :-
    prime_gen_value(PrimeGen, Res).

%-------------------------------------------------------------------------------

test :- euler(6, 13), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(10001, X), write('Result: '), writeln(X).