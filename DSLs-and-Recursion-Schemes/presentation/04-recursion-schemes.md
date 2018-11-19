# Recursion schemes

## Fixed Point

### Be it a function or a data type

```scala
def fix[A]: (A => A) => A = f => f(fix(f))

case class Fix[F[_]](unFix: F[Fix[F]]) extends AnyVal
```

| An $x \in X$ of a function $f: X \Rightarrow X$ is a
| fixed point if $f(x) = x$.

## Abstract Over Recursion

### Recursive grammar

```scala
sealed trait IntList

case class IntCons(i: Int, is: IntList) extends IntList
case object IntNil                      extends IntList
```
```scala
val intList = IntCons(3, IntCons(2, IntCons(1, IntNil)))
```

## Abstract Over Recursion

### Recursion can be abstracted away revealing the true primitives

1. Find a non-recursive grammar *precursor*
2. Find its fixpoint

## Abstract Over Recursion

### Non-recursive *precursor*

```scala
sealed trait IntListF[A]

object IntListF {
  case class IntConsF[A](i: Int, is: A) extends IntListF[A]
  case class IntNilF[A]()               extends IntListF[A]
}

type IntList = Fix[IntListF]

val intList1 = Fix(IntConsF(3, Fix(IntConsF(2, ... ))))
```

## Evaluation for Free

### A recipe for extracting a single value from an expression

```scala
val sum: IntList => Int = ???
```

## Evaluation for Free: Algebra

```scala
type Algebra: F[A] => A
```

*Algebra* (*F-Algebra*) consists of:

1. **F** is a **Functor**
2. **A** is its carrier type
3. Morphism **F[A]** to **A**

## Evaluation for Free: Algebra

### There can be many algebras

```scala
def sumAlgebra: Algebra[IntListF, Int] = {
  case IntConsF(i, is) => i + is
  case IntNilF()       => 0
}
```

```scala
def printAlgebra: Algebra[IntListF, String] = {
  case IntConsF(i, is) => s"[$i $is]"
  case IntNilF()       => ""
}
```

## Evaluation for Free: Initial Algebra

### One kind of Algebra to rule them all

```scala
type InitialAlgebra[F[_]] = Algebra[F, Fix[F]]
```

It is at least as powerful as all other *Algebra*s

```scala
val addOneAlgebra: InitialAlgebra[IntListF] = {
  case IntConsF(i, is) => Fix(IntConsF(i + 1, is))
  case IntNilF()       => Fix(IntNilF())
}
```

## Evaluation for Free: Initial Algebra

### The property to die for: Composition

```scala
def compose[F[_], A](
    phi: InitialAlgebra[F],
    psi: InitialAlgebra[F]
): InitialAlgebra[F] = phi compose unFix compose psi
```

```scala
val addTwoAlgebra: InitialAlgebra[IntListF] =
  compose(addOneAlgebra, addOneAlgebra)
```

## Evaluation for Free: Evaluate

### Unpack Traverse Apply

```scala
def cata[F[_]: Functor, A](
  algebra: Algebra[F, A]
)(
  fix: Fix[F]
): A = algebra(fix.unFix map cata(algebra))

val addTwo = cata(addTwoAlgebra)(expr)
```
