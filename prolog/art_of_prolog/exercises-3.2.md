Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 3, section 2
### 1. Variant of `sublist`

The goal `subsequence(Xs, Ys)` is in the program meaning if `Ys` contains the
elements of `Xs` and in the same order as they are in `Xs`. However, they don't
have to be consecutive, as with `sublist`.

For instance, the following goals are in the program meaning:
  * `subsequence([1 2 3], [a 1 2 b 3 c])`
  * `subsequence([1 2], [a 1 1 b 2])`

---
### 2. `adjacent` and `last`
```
adjacent(X, Y, [X, Y | Z]).
adjacent(X, Y, [_|Z]) ←  adjacent(X, Y, Z).

last(E, [E]).
last(E, [_|Xs]) ← last(E, Xs).
```

---
### 3. Doubling list
```
double([], []).
double([X|Xs], [X, X|XXs]) ← double(Xs, XXs).
```

---
### 4. Proof Tree Size of `reverse`
According Figure 3.5, the proof tree of program 3.16a has $\Sigma_{i=1}^{n+1} i
= (n+2)(n+1)/2$ nodes for a first argument of length $n$, while the proof tree
of program 3.16b has $n+2$ nodes.

---
### 5. Sum of an Integer List
### a. Using `plus`/3
```
sum([], 0).
sum([I|Is], Sum) ← sum(Is, S), plus(I, S, Sum).
```

### b. Without Auxiliary Predicate
```
sum([], 0).
sum([0|Is], Sum) ← sum(Is, Sum).
sum([s(I)|Is], s(Sum)) ← sum([I|Is], Sum).
```

---
Back to the [Table of Contents](Contents.md).