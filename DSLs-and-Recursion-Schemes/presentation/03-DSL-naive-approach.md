# DSL For Help

## DSL

### Arithmetics is recursive in its nature

```scala
sealed trait Expr

object Expr {
  final case class Op(op: OpType, opl: Expr, opr: Expr)
    extends Expr
  final case class Const(lit: Literal) extends Expr
  final case class Var(name: String) extends Expr
}
```

## DSL

```scala
sealed trait OpType

object OpType {
  final case object Add extends OpType
  final case object Eq  extends OpType
}
```
```scala
sealed trait Literal

object Literal {
  final case class IntLit(int: Int) extends Literal
  final case class BoolLit(bool: Boolean) extends Literal
}
```

## DSL: Optimizer

### Stick to the math

The $(Int, \plus)$ $(Int, =)$ $(Bool, =)$ algebras should be optimized

```scala
val optimize: Expr => Expr = {
  // Recurse: BOILERPLATE...
  case Op(op, lop, rop) =>
    optimize(Op(op, optimize(lop), optimize(rop)))
  // Many more cases...
  case ...
}
```

## DSL: Variable Substitution

Before *DSL* expression evaluation *variables* should be substituted

```scala
type Env = Map[String, Expr]

def subst: Env => Expr => Expr = env => {
  case v@Var(name)      => env get name getOrElse v
  // Recurse: BOILERPLATE...
  case Op(op, lop, rop) =>
    Op(op, subst(env)(lop), subst(env)(rop))
  case e: Expr          => e
}
```

## DSL: Evaluation

```scala
def eval: Env => Expr => Expr =
  subst(_) andThen optimize
```

## Limits of this model

Inherent recursive nature of a data type **always** has limits

- It does not compose well
- *optimize* is way too clomplex
- *eval* traverses expression twice
