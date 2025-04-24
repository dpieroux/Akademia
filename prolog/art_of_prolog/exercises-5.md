Back to the [Table of Contents](Contents.md).

# Solutions to exercises of chapter 5

## Section 5.2

### 1. Terminating Domain for `plus`

The terminating domain of `plus` contains the goals of the form `plus(X, Y, Z)`
with `Y`=`s`$^n$(0).

---
### 2. Complete binary tree

The complete binary tree are the closure of the singleton tree set {void} with
respect to the second clause of the `binary_tree` procedure. In practice, the
domain contains all of the finite trees whose leafs are all equal to `void`.

An incomplete binary tree is either infinite, or has at least a leaf variable.

## Section 5.3
### 1. Size of `append`
Let `append(Ns, Ms, Ls)` be in the meaning of `append` with `Ns` and `Ms` be
lists of length N and M respectively.

The size of that goal is equal to the 1 plus the size of `Ns`, `Ms` and `Ls`. As
`Ls` is list of length N+M, it comes:
* size of `Ns` = 2N+1
* size of `Ms` = 2M+1
* size of `Ls` = 2(M+N)+1
* size of `append(Ns, Ms, Ls)` = 1 + (2N+1) + (2M+1) + 2(M+N)+1 = 4(N+M+1)

A proof tree has N+1 nodes.

---
### 2. Linear Complexity of `plus`

The program `plus` stops once the first argument reaches 0; thus the proof
length of `plus(X, Y, Z)` for X=s$^i$(0) is $i$+1 = size(X).

As n := size(`plus(X, Y, Z)`) = 1+size(X)+size(Y)+size(Z) ≥ 3+size(X), it comes
that there is a proof of G of `plus(X, Y, Z)` with length = size(X) ≤ n-3. Thus
L(n) = n-3. As the consecutive goals are simpler than the original one, we have
also G(n) = n-3. And because the program is linearly recursive its
depth-complexity is also D(n)=n-3.

---
Back to the [Table of Contents](Contents.md).