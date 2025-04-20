Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 2

## 2.1

### 1. Sister and co
```
sister(Sister, Sibling) ←
  parent(Parent, Sister), parent(Parent, Sibling),
  female(Sister), Sister ≠ Sibling.

niece(Niece, Person) ←
  parent(Parent, Niece), sibling(Parent, Person), female(Niece).

sibling(Sib1, Sib2) ←
  father(Father, Sib1), father(Father, Sib2),
  mother(Mother, Sib1), mother(Mother, Sib2),
  Sib1 ≠ Sib2.
```

### 2. Parents in Law
```
mother_in_law(MiL, Husband) ←
  married_couple(Wife, Husband), mother(MiL, Wife).
mother_in_law(MiL, Wife) ←
  married_couple(Wife, Husband), mother(MiL, Husband).

brother_in_law(BiL, Husband) ←
  married_couple(Wife, Husband), brother(BiL, Wife).
brother_in_law(BiL, Wife) ←
  married_couple(Wife, Husband), brother(BiL, Husband).

son_in_law(SiL, Husband) ←
  married_couple(Wife, Husband), son(SiL, Wife), not(son(Son, Husband)).
son_in_law(SiL, Wife) ←
  married_couple(Wife, Husband), son(SiL, Husband), not(son(Son, Wife)).
```

### 3. Layout Description

```
left_of(pen, hourglass).
left_of(hourglass, butterfly).
left_of(butterfly, fish).
above(bicycle, pen).
above(camera, butterfly).

right_of(Right, Left) ← left_of(Left, Right).
below(Below, Above) ← above(Above, Below).
```

## 2.2

### 1. Course Organisation

```
location(Course, Building) ←
  course(Course, Time, Lecturer, location(Building, Room)).
busy(Lecturer, Time) ← course(Course, Time, Lecturer, Location).
cannot_meet(Lecturer1, Lecturer2) ←
  course(Course1, Time, Lecturer1, Location1),
  course(Course2, Time, Lecturer2, Location2),
```

For the last rule, the intended meaning is that two lecturers cannot meet if
they teach at the same time.

### 2. Schedule Conflict
```
schedule_conflict(Time, Place, Course1, Course2) ←
  course(Course1, time(Day, Start1, Finish1), Lecturer1, Place),
  course(Course2, time(Day, Start2, Finish2), Lecturer2, Place),
  Start1 <= Start2, Start2 <= Finish1.
schedule_conflict(Time, Place, Course1, Course2) ←
  course(Course1, time(Day, Start1, Finish1), Lecturer1, Place),
  course(Course2, time(Day, Start2, Finish2), Lecturer2, Place),
  Start2 <= Start1, Start1 <= Finish2.
```

### 3. Meeting the College Requirements.
```
result(alice, physics-1, 16).
result(alice, chemistry-1, 12).
result(bob, literature-2, 14).
<...>
result(zebulon, mathematics-1, 9).

failed(Student) ← result(Student, Course, Grad), Grad <= 10.
success(Student) ← not(failed(Student)).
```

Intended meaning: to succeed their academic year, students must have at least 10
in every coarse they followed.

### 4. My Little Geometric Database
```
mk_vector(point(X1, Y1), point(X2, Y1), vector(X, Y)) ←
  sub(X2, X1, X), sub(Y2, Y1, Y).

norm(vector(X, Y)) ←
  square(X, X2), square(Y, Y2), add(X2, Y2, Norm2), sqrt(Norm2, Norm).

distance(P1, P2, Distance) ←
  mk_vector(P1, P2, Vector), norm(Vector, Distance).

perimeter(shape(P1, P2, P3), Perimeter) ←
  distance(P1, P2, D3),
  distance(P2, P3, D1),
  distance(P3, P1, D2),
  add(D1, D2, Temp),
  add(Temp, D3, Perimeter).

triangle(shape(P1, P2, P3)).
quadriLateral(shape(P1, P2, P3, P4)).
```

## 2.3

### 1. Block Stack
```
above(Above, Below) ← on(Above, Below).
above(Above, Below) ← above(Above, X), on(X, Below).
```

### 2. Higher Blocks
```
left_on(X, Z) ← left_on(X, Y), left_on(Y, Z).
above(X, Z) ← above(X, Y), above(Y, Z).
higher(X, Z) ← above(X, Y), left_on(Y, Z).
higher(X, Z) ← above(X, Y), left_on(Z, Y).
```
Note: the intended meaning of higher is limited to 2 lines.

### 3. Counting `connected` Proof Tree Nodes
```
connected(a, e)
  edge(a, b)
  connected(b, e)
    edge(b, d)
    connected(d, e)
      edge(d, e)
      connected(e, e)
```

The proof tree of ```connected(a, e)``` has thus 7 nodes.

A proof tree for a path of $n$ intermediate graph nodes has $3+2n$ nodes. In the
example above, there are 2 intermediate nodes (```b``` and ```d```) and 7 nodes
in the proof tree.

---
Back to the [Table of Contents](Contents.md).