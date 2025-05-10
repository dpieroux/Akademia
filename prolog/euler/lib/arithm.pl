:- module(arithm, [prime_factors/2]).

/**
 * Arithmetic functionalities
 * --------------------------
 *
 * The module offers the following routines:
 *
 *  * prime_factors(+N, -PrimeFactors): prime factorization of the natural 'N'.
 *      'PrimeFactors' is the list of pairs (Pi, Ei) with prime numbers Pi in
 *      decreasing order and positive integers Ei such that 'N' is equal to the
 *      product of the terms Pi^Ei.
 *
 *  * divisors(+N, -Divisors): 'Divisors' is the list of all the divisors of
 *      'N'. The divisors are not returned in order; however the first one is
 *      'N' and the last one is 1.
 */

:- use_module('prime_gen.pl').

%-------------------------------------------------------------------------------
% Prime factorization
%-------------------------------------------------------------------------------

/**
 * prime_factors(+N, -PrimeFactors)
 *
 * 'PrimeFactors' is the list of pairs (Pi, Ei) with prime numbers Pi in
 * decreasing order and positive integers Ei such that 'N' is equal to the
 * product of the terms Pi^Ei.
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

%-------------------------------------------------------------------------------
% Divisors
%-------------------------------------------------------------------------------

/*
 *  divisors(+N, -Divisors)
 *
 *  'Divisors' is the list of all the divisors of 'N'. The divisors are not
 *  returned in order, despite the first one is 'N' and the last one is 1.
 */

divisors(N, Divisors) :-
    prime_factors(N, PrimeFactors),
    divisors_from_prime_factors(PrimeFactors, Divisors).

/*
 *  divisors_from_prime_factors(+PrimeFactors, -Divisors)
 *
 *  'Divisors' is the list of all the divisors of the number represented by its
 *  prime factorization PrimeFactors. 
 */

divisors_from_prime_factors(PrimeFactors, Divisors) :- 
    divisors_from_prime_factors_iter(PrimeFactors, [1], Divisors).

divisors_from_prime_factors_iter([(P, E) | PrimeFactors], Acc, Divisors) :- 
    divisors_from_prime_factors_iter(PrimeFactors, Acc, Acc1),
    iter_over_single_prime(P, E, Acc1, Acc1, Divisors).

divisors_from_prime_factors_iter([], Divisors, Divisors).

iter_over_single_prime(P, E, OtherPowerDivisors, Acc, UpdatedDivisors) :-
    E > 0,
    E1 is E-1,
    iter_over_single_prime(P, E1, OtherPowerDivisors, Acc, Acc1),
    M is P**E,
    iter_over_single_prime_power(M, OtherPowerDivisors, Acc1, UpdatedDivisors).
    
iter_over_single_prime(_, 0, _, Divisors, Divisors).

iter_over_single_prime_power(PP, [N|Ns], Acc, [M | Updated]) :-
    M is PP*N,
    iter_over_single_prime_power(PP, Ns, Acc, Updated).

iter_over_single_prime_power(_, [], Result, Result).