Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 3, section 1
### 1. Axiomatization of <, > and ≥
```
0 < s(X).
s(X) < s(Y) ← X < Y.

X ≥ 0.
s(X) ≥ s(Y) ← X ≥ Y.

s(X) > 0.
s(X) > s(Y) ← X > Y.
```

These programs can be used with
* The two ground arguments to check if the corresponding relation holds.
* With one ground argument and a non-ground argument. The results of the query
  will be all the instances of the non-ground argument that fulfil the relation.
* With two non-ground arguments. The results of the query will be all the pairs
  of the instances of the arguments that fulfil the relation.

---
### 2. Correctness and Completeness
**Correctness**. Let X≤Y be in the program meaning. Then, either:
  * X=0 and Y is any natural number, which is correct as 0≤Y is a mathematical
    fact, and therefore it is in the intended meaning.
  * X=s(X') and Y=s(Y'), with X'≤Y'. The intended mathematical meaning is
    X=X'+1, Y=Y'+1, with X'≤Y' ⇒ X'+1≤Y'+1 ⇒ X≤Y; and therefore X≤Y is in the
    intended meaning.

**Completeness**. Let X≤Y be in the intended meaning; that is, X≤Y
mathematically speaking. Then either
  * X=0 and Y can be any natural number as 0≤Y. This case is covered by
    the first rule.
  * X≠0, and thus there is a natural X' such that X=X'+1. Thus X≤Y ⇒ X'+1≤Y. In
    particular, 1≤Y and thus Y≠0; as a consequence $\exist$ Y' such that Y=Y'+1;
    therefore X'+1≤Y'+1 ⇒ X'≤Y'. So, X≤Y with X≠0 implies X=s(X'), Y=s(Y') and
    X'≤Y'. As these relations corresponds to the body of the second rule, X≤Y is
    also in the program meaning.

---
### 3. Number of Proof Tree Node
We assume that n≤m, as it is the condition for the query s$^\text{n}$(0) ≤
s$^\text{m}$(0) to be in the program meaning.

The proof tree is given in the table below. It has n+2+(m-n)=m+2 nodes. The
first n nodes reduces the left hand term down to 0, the last 2+(m-n) nodes
reduces the right hand term.

| Iteration | Node                                  |
| --------- | ------------------------------------- |
| 1         | s$^\text{n}$(0) ≤ s$^\text{m}$(0)     |
| 2         | s$^\text{n-1}$(0) ≤ s$^\text{m-1}$(0) |
| ...       | ...                                   |
| n+1       | 0 ≤ s$^\text{m-n}$(0)$                |
| n+2       | natural_number(s$^\text{m-n}$(0))     |
| n+3       | natural_number(s$^\text{m-n-1}$(0))   |
| ...       | ...                                   |
| n+2+(m-n) | natural_number(0).                    |

---
### 4. Even and Odd
```
even(0).
odd(s(0)).
even(s(X)) ← odd(X).
odd(s(X)) ← even(X).
```
Alternative:

```
even(0).
even(s(s(0))) ← even(X).

odd(s(0)).
odd(s(s(0))) ← odd(X).
```

---
### 5. Fibonacci
```
fib(0, 1).
fib(1, 1).
fib(s(s(X)), F) ← fib(X, F1), fib(s(X), F2), plus(F1, F2, F).
```

---
### 6. Integer Division
`div(X, Y, Z)`: Z is the integer quotient of X by Y.
```
div(X, Y, 0) ← X<Y.
div(X, Y, s(Z)) ← X≥Y, plus(X', Y, X), div(X', Y, Z).
```

---
### 7. GCD
```
gcd(X, X, X).
gcd(X, Y, Z) ← X<Y, plus(X, Y', Y), gcd(X, Y', Z).
gcd(X, Y, Z) ← X>Y, plus(X', Y, X), gcd(X', Y, Z).
```

---
### 8. Natural Numbers Starting at 1
```
natural_number(1).
natural_number(1+X) ← natural_number(X).

1 ≤ X.
1+X ≤ 1+Y ← X≤Y.

plus(1, Y, 1+Y).
plus(1+X, Y, 1+Z) ← plus(X, Y, Z).

times(1, X, X).
times(1+X, Y, Z) ← times(X, Y, XY), plus(XY, Y, Z).

exp(1, X, X).
exp(N, 1, 1).
exp(N+1, X, Y) ← exp(N, X, X_N), times(X, X_N, Y).

factorial(1, 1).
factorial(1+X, Y) ← factorial(X, FX), times(1+X, FX).
```
The programs for minimum and mod are identical to their original version. The
program for ackermann does not exist in the absence of 0.
```
gcd(X, Y, Gcd) ← mod(X, Y, Z), gcd(Y, Z, Gcd).
gcd(X, X, X).
```

---
Back to the [Table of Contents](Contents.md).