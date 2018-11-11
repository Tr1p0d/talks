# DSLs for Help (the naive approach)

## Feature Predicate Language

> Are you an adult?

`$featureVal + years(18) >= $now`

or

```json
{
  "op": "geq",
  "lop": {
    "op": "add",
    "lop": { "type": "date-time", "name": "feature" },
    "rop": { "type": "period", "value": { "years": "18" }}
  },
  "rop": { "type": "date-time", "name": "now"}
}
```

## Feature Predicate Language

```scala
sealed trait Expr

object Expr {
  final case class Op(op: OpType, opl: Expr, opr: Expr)
    extends Expr
  final case class Const(lit: Literal) extends Expr
  final case class Var(name: String) extends Expr
}
```

## Feature Predicate Language

```scala
sealed trait OpType

object OpType {
  final case object Add extends OpType
  final case object Eq  extends OpType
  ...
}
```

## Operations performed upon this DSL

* When received we need to:
  - Validate,
  - Typecheck,
  - Optimize

* When evaluation is required:
  - Substitute variables,
  - Evaluate

## Operations performed upon this DSL

```scala
val optimize: Expr => Expr = {
  case Op(Eq, e1, e2) => optimize(e1) + optimize(e2)
  case ...
}

val subst: Env => Expr => Expr = ???

val eval: Env => Expr => Expr = (env =>
  optimize compose subst(env)
)
```

## Limits of this model

Unofortunately it does not compose well:

* `eval` requires to traverse `Expr` tree twice (`optimize`, `subst`)
* how can I decouple `optimizer` into separate pieces?
* coupling `eval` into single feature:
    - hard to test, it is utterly complex,
    - code duplicity (2x `optimize`)
