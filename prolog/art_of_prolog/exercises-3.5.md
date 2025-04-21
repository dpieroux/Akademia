Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 3, section 5
To keep things simple, we use the anonymous variable `_` as well as arithmetic
expressions.

## 1/ Normalised Addition
```
normalised(A+B) ← constant(A), normalised(B).
```

## 2/ Boolean Formula
```
boolean_formula(true).
boolean_formula(false).
boolean_formula(A ∧ B) ← boolean_formula(A), boolean_formula(B).
boolean_formula(A ∨ B) ← boolean_formula(A), boolean_formula(B).
boolean_formula(~A) ← boolean_formula(A).
```

## 3. Conjunctive Normal Form
A `atom(X)` predicate is provided, which is true of `X` is an atom.
```
literal(X) ← atom(X).
literal(~X) ← atom(X).

is_disjunctive_formula(X) ← literal(X).
is_disjunctive_formula(A ∨ B) ←
  is_disjunctive_formula(A), is_disjunctive_formula(B).

is_conjunctive_normal_form(X) ← is_disjunctive_formula(X).
is_conjunctive_normal_form(A ∧ B) ←
  is_conjunctive_normal_form(A), is_conjunctive_normal_form(B).
```

## 4. Moving Negations inside Conjunctions and Disjunctions
```
negation_inwards(F, F) ← literal(F).
negation_inwards(~(A ∧ B), notA ∨ notB)
  ← negation_inwards(~A, notA), negation_inwards(~B, notB).
negation_inwards(~(A ∨ B), notA ∧ notB).
  ← negation_inwards(~A, notA), negation_inwards(~B, notB).
```

## 5. Computing the Conjunctive Normal Form
```
conjunctive_normal_form(F, G) ←
  negation_inwards(F, F1),   % See exercise 4.
  disjunction_inwards(F1, G),

disjunction_inwards(F, F) ← literal(F).

disjunction_inwards(A ∨ B, A ∨ B) ←
  disjunction_inwards(A, A),
  disjunction_inwards(B, B).

disjunction_inwards(A ∨ B, P ∧ Q) ←
  disjunction_inwards(A, A1 ∧ A2),
  disjunction_inwards(A1 ∨ B, P),
  disjunction_inwards(A2 ∨ B, Q).

disjunction_inwards(A ∨ B, P ∧ Q) ←
  disjunction_inwards(B, B1 ∧ B2),
  disjunction_inwards(A ∨ B1, P),
  disjunction_inwards(A ∨ B2, Q).

disjunction_inwards(A ∧ B, A' ∧ B') ←
  disjunction_inwards(A, A'),
  disjunction_inwards(B, B').
```
## 6. Bags
```
bag(void).
bag(Element, Multiplicity, RestOfBag) ← natural(Multiplicity), bag(RestOfBag).

union(void, B, B).
union(bag(E, M, R), B, U) ← union(R, B, V), add(E, M, V, U).

inter(void, B, void).
inter(bag(E, M, R), B, U) ← inter(R, B, V), min(E, M, V, U).

substitute(_, _, void, void).
substitute(Old, New, bag(Old, M, R), bag(New, M, R)).
substitute(Old, New, bag(E, M, R), bag(E, M, R')) ←
  Old≠E, substitute(Old, New, R, R')).

list_to_bag([], void).
list_to_bag([L|Ls], B) ← list_to_bag(Ls, B'), add(L, 1, B', B).

tree_to_bag([], void).
tree_to_bag(tree(Elem, Left, Right), Bag) ←
  tree_to_bag(Left, Bag_Left),
  tree_to_bag(Right, Bag_Right),
  union(Bag_Left, Bag_Right, Bag')
  add(Elem, 1, Bag', Bag).

% Helper function
add(E, M, void, bag(E, M, void)).
add(E, M, bag(E, N, R), bag(E, N', R)) ← N' is M+N.
add(E, M, bag(F, N, R), bag(F, N, R')) ← E≠F, add(E, M, R, R').

min(E, M, void, void).
min(E, M, bag(E, N, R), bag(E, M, R)) ← N≥M.
min(E, M, bag(E, N, R), bag(E, N, R)) ← N<M.
min(E, M, bag(F, N, R), bag(F, N, R')) ← E≠F, min(E, M, R, R').
```

---
Back to the [Table of Contents](Contents.md).