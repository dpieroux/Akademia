% euler(N, Res) Res is the difference between the square of the sum of the first
%   N natural numbers and the sum of the squares of these same numbers. 

euler(N, Res) :-
    Res is N*(N-1)*(N+1)*(3*N+2)/12.

%-------------------------------------------------------------------------------

test :- euler(10, 2640), !, writeln('Test: OK').
test :- writeln('Test: NOK').

run :- euler(100, X), write('Result: '), writeln(X).