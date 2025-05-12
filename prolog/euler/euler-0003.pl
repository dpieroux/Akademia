use_module("lib/arithm.pl").

% euler(N, LPN)
%	LPN is the largest prime factor of N.
euler(N, LPN) :- 
    prime_factors(N, PrimeFactors),
    last(PrimeFactors, (LPN, _)).

test :- euler(13195, 29), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(600851475143, X), write('Result: '), writeln(X).