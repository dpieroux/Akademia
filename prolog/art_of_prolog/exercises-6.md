ack to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 6

## Section 6.1

### 1. Tracing `daughter`

```
daughter(X,haran)
  father(haran,lot)           X=lot
  female(lot)
    False
  father(haran,milcah)        X=milcah
  female(milcah)
    True: output: (X=milcah)
;
  father(haran,yiscah)        X=yiscah
  female(yiscah)
      True: output: (X=yiscah)
;
  false
```

---
### 2. Tracing Insertion `quicksort`
```
sort([3, 1, 2], Ys)
  sort([1, 2], Zs)
    sort([2], Z2s)
      sort([], Z3s)           Z3s=[]
      insert(2, [], Z2s)      Z2s=[2]
    insert(1, [2], Zs)
      1>2 => False
    insert(1, [2], Zs)        Zs=[1, 2]
      1≤2
  insert(3, [1, 2], Ys)       Ys=[1|Z4s]
      3>1
      insert(3, [2], Z4s)     Z4s=[2|Z5s]
        3>2
        insert(3, [], [3])    Z5s=[3]
          True: output: (Ys=[1, 2, 3])
;
          false
        3≤2: false
      3≤1: false
false.
```

---
### 3. Tracing Permutation Sort
```
sort([3,1,2], Ys)
  permutation([3,1,2], [Z1|Z1s])                        Ys=[Z1|Z1s]
    select(Z1, [3,1,2], Y1s)
    1: select(3, [3,1,2], [1, 2])                       Z1=3, Y1s=[1, 2]
       permutation([1, 2], [Z2|Z2s])                    Z1s=[Z2|Z2s]
       select(Z2, [1, 2], Y2s)
       1: select(1, [1, 2], [2])                        Z2=1, Y2s=[2]
          permutation([2], [Z3|Z3s])                    Z2s=[Z3|Z3s]
            select(Z3, [2], Y3s)
            1: select(2, [2], [])                       Z3=2
               permutation([], [])                      Z3s=[]
               ordered([3,1,2])
                 3≤1
                 FAIL
            2: select(Z3, [], Z4s)                      Y3s=[2, Z4s]
               FAIL
       2: select(Z2, [2], Z5s)                          Y2s=[1|Z5s]
          1: select(2, [2], [])                         Z2=2, Z5s=[]
             permutation([1], [Z5|Z5s])                 Z2s = [Z5|Z5s]
             select(Z5, [1], Y5s)
             1: select(1, [], [])                       Z5=1, Y5s=[]
                permutation([], [])                     Z5s=[]
                ordered([3, 2, 1])
                  3≤2
                  FAIL
             2: select(Z5, [], Z6s)                     Y5s=[1|Z6s]
                FAIL
          2: select(Z2, [], Z9s)                        Z2=2, Z5s=[2|Z7s]
             FAIL
    2: select(Z1, [3|[1,2]], [3|Y6s])                   Y1s=[3|Y6s]
         select(Z1, [1,2], Y6s)
         1: select(1, [1, 2], [2])                      Z1=1, Y6s=[2]
            permutation([3, 2], [Z8|Z8s])               Z1s=[Z8|Z8s]
              select(Z8, [3, 2], Y7s)
              1. select(3, [3, 2], [2])                 Z8=3, Y7s=[2]
                 permutation([2], [Z9|Z9s])             Z8s=[Z9|Z9s]
                   select(Z9, [2], Y8s)
                   1. select(2, [2], [])                Z9=2
                      permutation([], [])               Z9s=[]
                        ordered([1,3,2])
                           1≤3
                           ordered([3,2])
                             3≤2
                             FAIL
                   2. select(Z9, [], Z9s)               Y8s=[2|Z9s]
                      FAIL
              2. select(Z8, [2], Z10s)                  Y7s=[3|Z10s]
                 1. select(2, [2], [])                  Z8=2, Z10s=[]
                    permutation([3], [Z11|Z11s])        Z8s=[Z11|Z11s]
                      select(Z11, [3], Y11s)
                      1. select(3, [3], [])             Z11=3, Y11s=[]
                         permutation([], [])            Z11s=[]
                         ordered([1, 2, 3])
                            1≤2
                            ordered([2, 3])
                              2≤3
                              ordered([3])
                              SUCCESS
output:(1, 2, 3)
;
                      2. select(Z11, [], z12s)          Y11s=[z12|z12s]
                         FAIL
                 2. select(Z8, [], Z13s)                Z10s=[2|Z10s]
                    FAIL
         2: select(Z1, [2], Y8s)                        Y6s=[1|Y8s]
            1: select(2, [2], [])                       Z1=2, Y8s=[]
               permutation([3, 1], [Z14|Z14s])          Z1s=[Z14|Z14s]
                 select(Z14, [3, 1], Y14s)
                 1. select(3, [3, 1], [1])              Z14=3
                    permutation([1], [Z15|Z15s])        Z14s=[Z15|Z15s]
                      select(Z15, [1], Y15s)
                      1. select(1, [1], [])             Z15=1, Y15s=[]
                         permutation([], [])            Z15s=[]
                         ordered([2, 3, 1])
                           2≤3
                           ordered([3, 1])
                             3≤1
                             FAIL
                      2. select(ZA5, [], Y16s)          Y15s=[1|Y16s]
                         FAIL
                 2. select(Z14, [1], Y16s)              Y14s=[3|Y16s]
                    1. select(1, [1], [])               Z14=1, Y16s=[]
                       permutation([3], [Z16|Z16s])     Z14s=[Z16|Z16s]
                         select(Z16, [3], Y17s)
                         1. select(3, [3], [])          Z16=3, Y17s=[]
                            permutation([],[])          Z16s=[]
                              ordered([2, 1, 3])
                                2≤3
                                FAIL
                         2. select(Z16, [], Y18s)       Y17s=[3|Y18s]
                           FAIL
                    2. select(Z14, [], Y18s)            Y16s=[1, Y18s]
                       FAIL
            2: select(Z1, [], Y19s)                     Y8s=[2|Y19s]
              FAIL
```

---
Back to the [Table of Contents](Contents.md).