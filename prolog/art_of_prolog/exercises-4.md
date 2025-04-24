Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 4

## Section 4.1

### 1. Unification algorithm: Append example

Unify `append([b], [c, d], L)`and `append([X|Xs], Ys, [X|Zs])`.

```
0 Stack: [append([b], [c, d], L) = append([X|Xs], Ys, [X|Zs])]
  Subst: []

1 Pop: append([b], [c, d], L)=append([X|Xs], Ys, [X|Zs])
  The two terms of the equations have the same functor `append/3`
  Stack: [b]=[X|Xs], [c, d]=Ys, L=[X|Zs]
  Subst: []

2 Pop: [b]=[X|Xs] <=> .(b, [])=.(X, Xs)
  The two terms of the equations have the same functor `./3`.
  Stack: b=X, []=Xs, [c, d]=Ys, L=[X|Zs]
  Subst: []

3 Pop: b=X
  Change all occurrences of X into b in S and T.
  Add X=b to T
  Stack: []=Xs, [c, d]=Ys, L=[b|Zs]
  Subst: X=b

4 Pop: []=Xs
  Change all occurrences of Xs into [] in S and T.
  Add X=b to T
  Stack: [c, d]=Ys, L=[b|Zs]
  Subst: X=b, Xs=[]

5 Pop: [c, d]=Ys
  Change all occurrences of Ys into [c, d] in S and T.
  Add Ys=[c, d] to T
  Stack: L=[b|Zs]
  Subst: X=b, Xs=[], Ys=[c, d]

6 Pop: L=[b|Zs]
  Change all occurrences of L into [b|Zs]] in S and T.
  Add L=[b|Zs] to T
  Stack: []
  Subst: X=b, Xs=[], Ys=[c, d], L=[b|Zs]
```
Thus, a possible MGU for the problem is {X=b, Xs=[], Ys=[c, d], L=[b|Zs]}.

---
### 2. Most general unifier algorithm: Hanoi example

Unify `hanoi(s(N), A, B, C, Ms)` and `hanoi(s(s(0)), a, b, c, Xs)`.

```
0 Stack: [hanoi(s(N), A, B, C, Ms) = `hanoi(s(s(0)), a, b, c, Xs)]
  Subst: []

1 Pop: hanoi(s(N), A, B, C, Ms) = `hanoi(s(s(0)), a, b, c, Xs)
  The two terms of the equations have the same functor `hanoi/5`
  Stack: s(N) = s(s(0)), A=a, B=b, C=c, Ms=Xs
  Subst: []

2 Pop: s(N) = s(s(0))
  The two terms of the equations have the same functor `s/5`
  Stack: N=s(0), A=a, B=b, C=c, Ms=Xs
  Subst: []

3 Pop: N=s(0)
  Change all occurrences of N into s(0) in S and T.
  Stack: A=a, B=b, C=c, Ms=Xs
  Subst: [N=s(0)]

4 Pop: A=a
  Change all occurrences of A into a in S and T.
  Stack: B=b, C=c, Ms=Xs
  Subst: [N=s(0), A=a]

5 Pop: B=b
  Change all occurrences of B into b in S and T.
  Stack: C=c, Ms=Xs
  Subst: [N=s(0), A=a, B=b]

6 Pop: C=c
  Change all occurrences of C into c in S and T.
  Stack: Ms=Xs
  Subst: [N=s(0), A=a, B=b, C=c]

7 Pop: Ms=Xs
  Change all occurrences of Ms into Xs in S and T.
  Stack: []
  Subst: [N=s(0), A=a, B=b, C=c, Ms=Xs]
```
Thus, a possible MGU for the problem is {N=s(0), A=a, B=b, C=c, Ms=Xs}.

## Section 4.2

### 1. Tracing the Sort Algorithms
#### Permutation Sort
```
sort([3, 1, 2], Xs)
  permutation([3, 1, 2], Xs)    Xs=[Z1|Z1s]
    select(Z1, [3, 1, 2], Y1s)  Y1s=[3|Z2s]
      select(Z1, [1, 2], Z2s)   Z1=1; Z2s=[2]
    permutation([3,2], Z1s)     Z1s=[Z3|Z3s]
      select(Z3, [3,2], Y2s)    Y2s=[3|Z4S]
        select(Z3, [2], Z4s)    Z3=2; Z4s=[]
      permutation([3], Z3s)     Z3s=[Z4|Z4s]
        select(Z4,[3], Y3s)     Z4=3; Y3s=[]
        permutation([], Z4s)    Z4s=[]
  ordered([1, 2, 3])
    1≤2
    ordered([2, 3])
      2≤3
      ordered([3])

Output: Xs=[1, 2, 3].
```

#### Insert Sort
```
sort([3, 1, 2], Xs)
  sort([1, 2], Z1s)
    sort([2], Z2s)
      sort([], Z3s)             Z3s=[]
      insert(2, [], Z2s)        Z2s=[2]
    insert(1, [2], Z1s)
      1≤2                       Z1s=[1,2]
  insert(3, [1,2], Xs)          Xs=[1|Z4s]
    3>1
    insert(3, [2], Z4s)         Z4s=[2|Z5s]
      3>2
      insert(3, [], Z5s)        Z5s=[3]

Output: Xs=[1, 2, 3].
```

#### Quicksort
```
quicksort([3, 1, 2], Xs)
  partition([1, 2], 3, L1s, B1s)  L1s=[1|L3s]
    1≤3
    partition([2], 3, L3s, B1s)   L3s=[2|L4s]
      2≤3
      partition([], 3, L4s, B1s)  L4s=[]; B1s=[]
  quicksort([1, 2], L2s)
    partition([2], 1, L5s, B5s)   B5s=[2|B7s]
      2>1
      partition([], 1, L5s, B7s)  L5s=[]; B7s=[]
    quicksort([], L6s)            L6s=[]
    quicksort([2], B6s)
      partition([], 2, L8s, B8s)  L8s=[], B8s=[]
      quicksort([], L9s)          L9s=[]
      quicksort([], B9s)          B9s=[]
      append([], [2], B6s)        B6s=[2]
    append([], [1, 2], L2s)       L2s=[1, 2]
  quicksort([], B2s)              B2s=[]
  append([1, 2],[3], Xs)          Xs=[1|X1s]
    append([2], [3], X1s)         X1s=[2|X2s]
      append([], [3], X2s)        X2s=[3]
```
Output: Xs=[1, 2, 3].

### 2. Derivative Trace
We need to add a rule for numbers: `derivative(Y,X,0) ← number(Y)`.

```
derivative(3*sin(x)-4*cos(x),x,D)   D=D1-D2
  derivative(3*sin(x), x, D1)       D1=3*D4+D3*sin(x)
    derivative(3, x, D3)            D3=0
      number(3)
    derivative(sin(X), x, D4)       D4=cos(x)
  derivative(4*cos(x), x, D2)       D2=4*D6+D5*cos(x)
    derivative(4, x, D5)            D5=0
      number(4)
    derivative(cos(x), x, D6)       D6=-sin(x)
```
Output: D=(3$*$cos(x)+0$*$sin(x))-(4$*$(-sin(x))+0$*$cos(x)).

---
Back to the [Table of Contents](Contents.md).