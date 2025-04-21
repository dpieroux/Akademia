Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 3, section 3

### 1. `substitute` Program
```
substitute(X, Y, [X|L1], [Y|L2]) ← substitute(X,Y,L1,L2).
substitute(X, Y, [Y|L1], [Y|L2]) ← X≠Y, substitute(X,Y,L1,L2).
```

---
### 2. Variant of `select`
The meaning of the program is all the ground goals `select(X, Ys, Zs)` such that
the list `Ys` contains at least one element `X` and `Zs` is `Ys` without the
first occurrence of `X`.

---
### 3. Removing doubles
```
no_doubles([], []).
no_doubles([A|As], Bs) ← member(A, As), no_double(As, Bs).
no_doubles([A|As], [A|Bs]) ← not(member(A, As)), no_double(As, Bs).
```

---
### 4.Even and Odd Permutations
```
even_permutation(Xs, Xs).
even_permutation(Xs, Ys) ← transposition(Xs, Zs), odd_permutation(Zs, Ys).
odd_permutation(Xs, Ys) ← transposition(Xs, Zs), even_permutation(Zs, Ys).

transposition(L1XL2YL3, L1YL2XL3) ←
  append(L1, [X|L2YL3], L1XL2YL3), append(L2, [Y|L3], L2YL3),
  append(L1, [Y|L2XL3], L1YL2XL3), append(L2, [X|L3], L2XL3),
```

---
### 5. Merge Sort
```
merge_sort(Xs, Ys) ←
  split(Xs, [], [], X1s, X2s),
  merge_sort(X1s, Y1s),
  merge_sort(X2s, Y2s),
  merge(Y1s, Y2s, Ys).

split([], Acc1, Acc2, Acc1, Acc2).
split([X|Xs], Acc1, Acc2, Y1s, Y2s) ← split(Xs, Acc2, [X|Acc1], Y1s, Y2s).

merge([], X2s, X2s).
merge(X1s, [], X1s).
merge([X1|X1s], [X2|X2s], [X1|Ys]) ← X1<X2, merge(X1s, [X2|X2s], Ys).
merge([X1|X1s], [X2|X2s], [X2|Ys]) ← X1≥X2, merge([X1|XXs], X2s, Ys).
```

---
### 6. Finding the K$^{th}$ Largest Number using a median approximation
```
break5(Xs, [Xs]) ← length(Xs, Len), Len ≤ 5.
break5([X1, X2, X3, X4, X5 | Xs], [[X1, X2, X3, X4, X5] | X5ss) ←
  length(Xs, Len), Len > 1, break5(Xs, X5ss).

median5([X1], X1).
median5([X1, X2], X1).
median5([X1, X2, X3], Y2) ←
  permutation([X1, X2, X3], [Y1, Y2, Y3]), Y1≤Y2, Y2≤Y3.
median5([X1, X2, X3, X4], Y3) ←
  permutation([X1, X2, X3, X4], [Y1, Y2, Y3, Y4]), Y1≤Y2, Y2≤Y3, Y3≤Y4.
median5([X1, X2, X3, X4, X5], Y3) ←
  permutation([X1, X2, X3, X4, X5], [Y1, Y2, Y3, Y4, Y5]),
  Y1≤Y2, Y2≤Y3, Y3≤Y4, Y4≤Y5.

map_median5([X5s], [M]) ← median5(X5s, M).
map_median5([X5s|X5ss], [M|Ms]) ← median5(X5s, M), map_median5(X5ss, Ms).

median(Xs, M) ← length(Xs, LenXs), LenXs ≤ 5, median5(Xs, M),
median(Xs, M) ← length(Xs, LenXs), LenXs > 5,
  break5(Xs, X5ss), map_median5(X5ss, Ms), median(Ms, M).

kth_largest([X], 1, X).
kth_largest(Xs, K, X) ←
  median(Xs, M), partition(Xs, M, Ls, Bs),
  length(Ls, L), K≤L, kth_largest(Ls, K, X).
kth_largest(Xs, K, X) ←
  median(Xs, M), partition(Xs, M, Ls, Bs),
  length(Ls, L), K>L, plus(L, K', K), kth_largest(Bs, K', X).
```

---
### 7. Playing Poker
To keep things simple, we use the anonymous variable `_` as well as arithmetic expressions.

```
better_poker_hand(Hand1, Hand2, Hand1) ←
  sort(Hand1, H1), score(H1, S1),
  sort(Hand2, H2), score(H2, S2),
  better_score(S1, S2).

better_poker_hand(Hand1, Hand2, Hand2) ←
  sort(Hand1, H1), score(H1, S1),
  sort(Hand2, H2), score(H2, S2),
  better_score(S2, S1).

```
`sort(Hand, Cs)`: `Cs` are the cards of `Hand` sorted decreasingly by value.
```
sort(Hand, Cs) ← permutation(Hand, Cs), map_value(Cs, Vs), decreasing(Vs).

decreasing([X]).
decreasing([X,Y|Zs]) ← X≥Y, decreasing([Y|Zs]).
```

`score(Hand, Score)`: `Score` is the score associated to `Hand`. Such a score is
a list of numbers, and two scores have to be compared using lexicographic order.

```
score(Hand, [V]) ← has_straight_flush(Hand, V).

score(Hand, [0, V]) ←
  not(has_straight_flush(Hand, _)),
  has_four_of_a_kind(Hand, V).

score(Hand, [0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  has_full_house(Hand, [V1, V2]), V=(V1*20)+V2.

score(Hand, [0, 0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  not(has_full_house(Hand, _)),
  has_straight(Hand, V).

score(Hand, [0, 0, 0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  not(has_full_house(Hand, _)),
  not(has_straight(Hand, _)),
  has_flush(Hand),
  map_value(Hand, [V1, V2, V3, V4, V5]),
  V=((((V1*20)+V2)*20+V3)*20+V4)*20+V5.

score(Hand, [0, 0, 0, 0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  not(has_full_house(Hand, _)),
  not(has_straight(Hand, _)),
  not(has_flush(Hand, _)),
  has_three_of_a_kind(Hand, V).

score(Hand, [0, 0, 0, 0, 0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  not(has_full_house(Hand, _)),
  not(has_straight(Hand, _)),
  not(has_flush(Hand, _)),
  not(has_three_of_a_kind(Hand, _)),
  has_two_pairs(Hand, [V1, V2]), V=100*V1+V2.

score(Hand, [0, 0, 0, 0, 0, 0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  not(has_full_house(Hand, _)),
  not(has_straight(Hand, _)),
  not(has_flush(Hand, _)),
  not(has_three_of_a_kind(Hand, _)),
  not(has_two_pairs(Hand, _)),
  has_pair(Hand, V).

score(Hand, [0, 0, 0, 0, 0, 0, 0, 0, V]) ←
  not(has_straight_flush(Hand, _)),
  not(has_four_of_a_kind(Hand, _)),
  not(has_full_house(Hand, _)),
  not(has_straight(Hand, _)),
  not(has_flush(Hand, _)),
  not(has_three_of_a_kind(Hand, _)),
  not(has_two_pairs(Hand, _)),
  not(has_pair(Hand, _)),
  map_value(Hand, [V1, V2, V3, V4, V5]),
  V=((((V1*20)+V2)*20+V3)*20+V4)*20+V5.

```
`better_score(A, B)` is true if `A` is a better score than `B`.
```
better_score([A|As], [B|Bs]) ← A>B.
better_score([A|As], [B|Bs]) ← A=B, better_score(As, Bs).

```
`value(C, V)`: `V` is the numerical value of the card `C`.
```
value(card(S, V), V) ← 2≤V, V≤10.
value(card(S, jack), 11).
value(card(S, queen), 12).
value(card(S, king), 13).
value(card(S, ace), 14).
```
`map_value(Cs, Vs)`: `Vs` is the sequence of numerical values of the list of
cards `Cs`.
```
map_value([], []).
map_value([C|Cs], [V|Vs]) ← value(C, V), map_value(Cs, Vs).

```
`suit(C, S)`: `S` is the suit of the card `C`.
```
suit(card(S, V), S).
```
`map_value(Cs, Vs)`: `Vs` is the sequence of suits of the list of cards `Cs`.
```
map_suit([], []).
map_suit([C|Cs], [S|Ss]) ← suit(C, S), map_suit(Cs, Ss).
```
`has_pair(Cs, V)` if the cards `Cs` has a pair of `V`.

`has_pair(Cs, V, Cs')` if the cards `Cs` has a pair of `V` and `Cs'` is `Cs`
without the pair.
```
has_pair(Cs, V) ← has_pair(Cs, V, Cs').
has_pair([card(S, V)|Cs], V, Cs') ← select(card(S', V), Cs, Cs').
has_pair([card(S, V)|Cs], V, [card(S,V)|Cs']) ← has_pair(Cs, V, Cs').
```
`has_two_pair(Cs, [V1, V2])` if the cards `Cs` has a pair of `V1` and a pair of
`V2`.
```
has_two_pairs(Cs, [V1, V2]) ← has_pair(Cs, V1, Cs'), has_pair(Cs', V2), V1≠V2.
```
`has_three_of_a_kind(Cs, V)` if the cards `Cs` has three cards of value `V`.

`has_three_of_a_kind(Cs, V, Cs')` if the cards `Cs` has three cards of value `V`
and `Cs'` is `Cs` without these 3 cards.
```
has_three_of_a_kind(Cs, V) ← has_three_of_a_kind(Cs, V, _).
has_three_of_a_kind(Cs, V, Cs') ←
  has_pair(Cs, V, Cs''), select(card(S3, V), Cs'', Cs').
```
`has_flush(Cs, Vs)` if the cards `Cs` are a flush of values `Vs`.
```
has_flush(Cs, Vs) ← map_suit(Cs, [S, S, S, S, S]), map_value(Cs, Vs).
```
`has_straight(Cs, V)` if the cards `Cs` form a straight whose highest card has
value `V`.
```
has_straight(Cs, V1) ← map-value(Cs, [V1, V2, V3, V4, V5]),
  V1=V2+1, V2=V3+1, V3=V4+1, v4=V5+1.
```
`has_full_house(Cs, [V1, V2])` if the cards `Cs` have 3 cards of value `V1` and
2 cards of value `V2`.
```
has_full_house(Cs, [V1, V2]) ←
  has_three_of_a_kind(Cs, V1, Cs'), has_pair(Cs', V2).
```
`has_four_of_a_kind(Cs, V)` if the cards `Cs` have 4 cards of value `V`.
```
has_four_of_a_kind(Cs, V) ← has_pair(Cs, V, Cs'), has_pair(Cs', V, _).
```
`has_four_of_a_kind(Cs, V)` if the cards `Cs` forms a straight flush with
highest card `V`.
```
has_straight_flush(Cs, V) ← has_straight(Cs, V), has_flush(Cs, Vs).
``````
---
Back to the [Table of Contents](Contents.md).