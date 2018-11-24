# Recursion schemes

## Fixed Point

### Be it a function or a data type

```scala
def fix[A]: (A => A) => A = f => f(fix(f))
```
### or a data type
```scala
case class Fix[F[_]](unFix: F[Fix[F]]) extends AnyVal
```

## Abstract Over Recursion

### Reveal the true primitives

```scala
sealed trait ExprF[A]

object ExprF {
  case class Op[A](op: OpType, opl: A, opr: A)
    extends ExprF[A]
  case class Const[A](lit: Literal) extends ExprF[A]
  case class Var[A](name: Name) extends ExprF[A]
}

type Expr = Fix[ExprF]
```

## Abstract Over Recursion

### Fix In Action

![](./img/fix-tree.png)

## Evaluation for Free

### extract a single value from an expression

Unpack Traverse Apply

```scala
def cata[F[_]: Functor, A](
  algebra: F[A] => A
)(
  fix: Fix[F]
): A = algebra(fix.unFix map cata(algebra))

val some: Expr => A = cata(someAlgebra)(_)
```

## Evaluation for Free: Algebra

```scala
type Algebra: F[A] => A
```

*Algebra* (*F-Algebra*) consists of:

1. An **(Endo-)Functor** **F**
2. Its carrier type **A**
3. A Morphism **F[A]** to **A**

## Evaluation for Free: Algebra

### There can be many algebras

```scala
val printAlgebra: Algebra[ExprF, String] = {
  case Op(Eq, lop, rop)  => s"($lop == $rop)"
  case Op(Add, lop, rop) => s"($lop + $rop)"
  case Const(lit)        => s"$lit"
  case Var(variable)     => s"var($variable)"
}
```

## Evaluation for Free: Initial Algebra

### One kind of Algebra to rule them all

```scala
type InitialAlgebra[F[_]] = Algebra[F, Fix[F]]
```

It is at least as powerful as all other *Algebra*s

```scala
val optimizeIntEqA: InitialAlgebra[ExprF] = {
  case Op(Eq, ConstInt(i1), ConstInt(i2)) => bool(i1 == i2)
  case e: ExprF[Expr] => Fix(e)
}
```

## Evaluation for Free: Initial Algebra

### The property to die for

```scala
def compose[F[_], A](
    phi: InitialAlgebra[F],
    psi: InitialAlgebra[F]
): InitialAlgebra[F] = phi compose unFix compose psi
```
