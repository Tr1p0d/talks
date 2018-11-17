# Recursion schemes

## Fixed point (Fixpoint)

> TODO
>
> -- Wikipedia

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

Recursion can be abstracted away revealing the true primitives.

1. Find a non-recursive grammar *precursor*
2. Find its fixed-point data type

## Abstract Over Recursion

### Non-recursive *precursor*

```scala
sealed trait IntListF[A]

case class IntConsF[A](i: Int, is: A) extends IntListF[A]
case class IntNilF[A]()               extends IntListF[A]
```
### Fixed point data type

```scala
case class Fix[F[_]](unFix: F[Fix[F]]) extends AnyVal
```

### Grammar

```scala
type IntList = Fix[IntListF]
```
```scala
val intList1 = Fix(IntConsF(3, Fix(IntConsF(2, ... ))))
```

## Evaluation for Free

> Evaluation is a recipe for extracting a single value from an expression.
>
> -- Bartosz Milewski

```scala
val sum: IntList => Int = ???
```

In order to do that we need to:

1. Find a recipe to produce a single value from `IntListF` and `Fix`
2. Combine them

## Evaluation for Free: Algebra

Function of type:

```scala
type Algebra: F[A] => A
```

is called an `Algebra` (`F-Algebra`) where:

1. `F[_]` is a functor
2. `A` is a carrier type

## Evaluation for Free: Algebra

There are many `Algebras` based on a given `IntListF[_]`

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

But there is one algebra to rule them all. The `InitialAlgebra`:

```scala
type InitialAlgebra[F[_]] = Algebra[F, Fix[F]]
```

1. It is not lossy
    - Always preservers the structure
    - Does not produce a summary
2. It is at least as powerful as all other algebras

```scala
val addOneAlgebra: InitialAlgebra[IntListF] = {
  case IntConsF(i, is) => Fix(IntConsF(i + 1, is))
  case IntNilF()       => Fix(IntNilF())
}
```

## Evaluation for Free: Initial Algebra

The property to die for: `InitialAlgebra`s do compose:

```scala
def compose[F[_], A](
    phi: InitialAlgebra[F],
    psi: InitialAlgebra[F]
): InitialAlgebra[F] =
  phi compose unFix compose psi
```

```scala
val addTwoAlgebra: InitialAlgebra[IntListF] =
  compose(addOneAlgebra, addOneAlgebra)
```

## Evaluation for Free: Eval

TODO: Introduce catamorphism... possibly via

1. Initial algebra Homomorphism proof
2. Or a diagram
