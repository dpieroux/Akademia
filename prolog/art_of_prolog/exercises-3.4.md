Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 3, section 4
To keep things simple, we use the anonymous variable `_` as well as arithmetic expressions.

## 1. Subtree of a Tree
```
subtree(T, T).
subtree(S, tree(E, L, R)) ← subtree(S, L).
subtree(S, tree(E, L, R)) ← subtree(S, R).
```

## 2.  Summing the Tree Nodes
```
sum_tree(void, 0).
sum_tree(tree(E, L, R), S) ← sum_tree(L, SL), sum_tree(R, SR), S is E+SL+SR.
```

## 3. Ordered Tree
```
ordered(void).
ordered(tree(X, L, R)) ← ordered_left(X, L), ordered_right(X, R).

ordered_left(X, tree(Y, L, R)) ← Y≤X, ordered(tree(Y, L, R)).
ordered_right(X, tree(Y, L, R)) ← X≤Y, ordered(tree(Y, L, R)).
```

## 4. Inserting into an Ordered Tree
```
tree_insert(X, void, tree(X, void, void)).
tree_insert(X, tree(X, L, R), tree(X, L, R)).
tree_insert(X, tree(Y, L, R), tree(Y, L', R)) ← X<Y, tree_insert(X, L, L').
tree_insert(X, tree(Y, L, R), tree(Y, L, R')) ← X>Y, tree_insert(X, R, R').
```

## 5. Path to an Element
`path(Elem, Tree, Path)` holds if there is a path `Path` from the tree root to
an element `Elem`. `Path` contains the atom `left` and `right` indicating which
branch to follow at each step. An empty path indicates that `Elem` is the root
element.
```
path(E, tree(E, L, R), []).
path(E, tree(X, L, R), [left|Path])  ← E≠X, path(E, L, Path).
path(E, tree(X, L, R), [right|Path]) ← E≠X, path(E, R, Path).
```
---
Back to the [Table of Contents](Contents.md).