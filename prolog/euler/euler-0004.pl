:- use_module('lib/arithm.pl').

% euler(Inf, Sup, Res)
%	Res is the largest palindrome which is the product of two numbers included
%	in [Inf Sup].

euler(Inf, Sup, Res) :- search_divisors(Inf, Sup, Sup, Sup, 0, Res).

search_divisors(Inf, Sup, D1, D2, Best, Res) :-
    D2 >= D1,
    Cur is D1*D2,
    (is_palindrome(Cur) -> NewBest = Cur ; NewBest = Best),
    D4 is D2-1,
    search_divisors(Inf, Sup, D1, D4, NewBest, Res).

search_divisors(Inf, Sup, D1, D2, Best, Res) :-
    D2 < D1,
    D3 is D1-1,
    D3 * Sup > Best,
    search_divisors(Inf, Sup, D3, Sup, Best, Res).

search_divisors(Inf, Sup, D1, D2, Best, Best) :-
    D2 < D1,
    D3 is D1-1,
    (D3 * Sup =< Best; D3 < Inf).

is_palindrome(N) :- number_digits(N, Ds), reverse(Ds, Ds).

%-------------------------------------------------------------------------------

test :- euler(10, 99, 9009), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(100, 999, X), write('Result: '), writeln(X).