:- module(arithm, [prime_factors/2]).

/**
 * Basic arithmetic properties
 * ---------------------------
 *
 * The module offers the following routines:
 *
 *  * prime_factors(+N, -PrimeFactors): prime factorization of the natural N.
 *      PrimeFactors is the unique list of pairs (P_i, E_i) of prime numbers
 *      P_i in descending order, E_i strictly positive exponents, and N equal to
 *      the product of the terms P_i^E_i.
 */

:- use_module('prime_gen.pl').

%-------------------------------------------------------------------------------
% Prime factorization
%-------------------------------------------------------------------------------

/**
 * prime_factors(+N, -PrimeFactors)
 *
 * PrimeFactors is the unique list of pairs (P_i, E_i) of prime numbers P_i in
 * descending order, E_i strictly positive exponents, and N equal to the product
 * of the terms P_i^E_i.
 */

prime_factors(N, PrimeFactors) :-
    prime_gen_new(PrimeGen),
    prime_factors(N, PrimeGen, [], PrimeFactors).

prime_factors(N, PGen, Acc, PrimeFactors) :-
    N > 1,
    prime_gen_value(PGen, P),
    P*P =< N,
    !,
    factor_exponent(N, P, N1, E),
    add_factor(P, E, Acc, Acc1),
    prime_gen_next(PGen, PGen1),
    prime_factors(N1, PGen1, Acc1, PrimeFactors).

prime_factors(N, _, Acc, [(N, 1) | Acc]) :- N > 1.

prime_factors(1, _, Acc, Acc).


/**
 * factor_exponent(+N, +M, -N1, -E)
 *
 * E is the largest power of M such that M^E divides N; N1 is the quotient of N
 * by M^E, that is: N = N1 * M^E.
 */

factor_exponent(N, M, N1, E) :- factor_exponent(N, M, 0, N1, E).

factor_exponent(N, M, Acc, N1, E) :-
    divmod(N, M, Quotient, 0), 
    !, 
    Acc1 is Acc+1,
    factor_exponent(Quotient, M, Acc1, N1, E).

factor_exponent(N, _, Acc, N, Acc).


/**
 * add_factor(+P, +E, +Acc, -Acc1)
 *
 * Acc1 is the result of prepending the pair (P, E) to the list Acc if E>0;
 * Acc1 unifies to Acc if E=0.  
 */
add_factor(P, E, Acc, [(P, E) | Acc]) :- E>0.
add_factor(_, 0, Acc, Acc).