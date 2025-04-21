% sum_multiples(M, Bound, S)
%   S is the sum of the multiples of M smaller than Bound.

sum_multiples(M, Bound, S) :-
	N is (Bound - 1) div M,
	S is M*N*(N + 1)/2.

% euler(Bound, Sum)
%   Sum is the sum of the multiples of 3 or 5 below Bound.
euler(Bound, Sum) :-
	sum_multiples(3, Bound, N3),
	sum_multiples(5, Bound, N5),
	sum_multiples(15, Bound, N15),
	Sum is N3 + N5 - N15.

test :- euler(10, 23), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(1000, X), write('Result: '), writeln(X).