% Fact: the sequence of even Fibonacci numbers, e(i), starting from zero, is:
% e(0)=0, e(1)=2, e(n+2) = 4*e(n+1)+e(n).

% euler(Bound, Sum)
%		S is the sum of the even Fibonacci numbers not exceeding Bound.
euler(Bound, Sum) :- sum_even_fib(0, 2, Bound, 0, Sum).

sum_even_fib(_, E1, Bound, Acc, Acc) :- E1 > Bound.

sum_even_fib(E0, E1, Bound, Acc, Sum) :-
	E1 =< Bound,
	E2 is 4*E1+E0,
	Acc1 is Acc+E1,
	sum_even_fib(E1, E2, Bound, Acc1, Sum).

test :- euler(100, 44), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(4000000, X), write('Result: '), writeln(X).