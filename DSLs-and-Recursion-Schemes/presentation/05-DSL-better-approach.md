# DSL for Help: Revisited

## DSL - Revisited

### In search for a Fixpoint

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

## DSL - Revisited: Optimizer

### Composition for the win

```scala
val optimizeIntAddA: InitialAlgebra[ExprF] = {
  case Op(Add, ConstInt(i1), ConstInt(i2)) =>
    int(i1 + i2)
  case e: ExprF[Expr] => Fix(e)
}
```

```scala
val optimizeA: InitialAlgebra[ExprF] = andThenAll(
  List(optimizeIntEqA, optimizeIntAddA, optimizeBoolEqA)
)
```

## DSL - Revisited: Substitution

### No more unnecessary recursion

```scala
val substA: Env => InitialAlgebra[ExprF] = env => {
  case v@Var(name)    => env get name getOrElse Fix(v)
  case e: ExprF[Expr] => Fix(e)
}
```

## DSL - Revisited: Evaluation

### Everything and nothing has changed

```scala
val evalA: Env => InitialAlgebra[ExprF] =
  env => andThen(substA(env), optimizeA)
```

```scala
val eval: Env => Expr => Expr = env => _ cata evalA(env)
```

## How About Typechecker

### Bad news

Is **not total** nor can be expressed by means of **cata**

```scala
def typeCheck[F[_]: Monad]: Expr => F[Expr]
```

### Good news

There is an **InitialMAlgebra** and there is entire **ZOO** of morphisms

