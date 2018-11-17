package plain.recursion

object intList {
  import IntList._

  sealed trait IntList

  object IntList {
    final case class IntCons(i: Int, is: IntList) extends IntList
    final case object IntNil                     extends IntList
  }

  val intNil: IntList = IntNil
  def intCons(i: Int, is: IntList): IntList = IntCons(i, is)

  val intList: IntList = intCons(3, intCons(2, intCons(1, intNil)))
}

object expr {
  import Literal._
  import OpType._
  import Expr._

  type Name = String

  type Env = Map[String, Expr]

  sealed trait Literal

  object Literal {
    final case class IntLit(int: Int) extends Literal
    final case class BoolLit(bool: Boolean) extends Literal
  }

  sealed trait OpType

  object OpType {
    final case object Eq extends OpType
    final case object Add extends OpType
  }

  sealed trait Expr

  object Expr {
    final case class Op(op: OpType, opl: Expr, opr: Expr) extends Expr
    final case class Const(lit: Literal) extends Expr
    final case class Var(name: Name) extends Expr
  }

  val int: Int => Expr = i => Const(IntLit(i))
  val bool: Boolean => Expr = b => Const(BoolLit(b))
  val variable: Name => Expr = Var.apply
  val equal: Expr => Expr => Expr = lop => rop => Op(Eq, lop, rop)
  val add: Expr => Expr => Expr = lop => rop => Op(Add, lop, rop)

  /** Optimizer
    *
    * Optimizes given [[Expr]] expression based on common algebraic laws with
    * respect to (Int, +), (Int, ==), (Boolean, ==) algebras.
    *
    * @note In case of this particular [[Expr]] algebra the optimizer is
    * very complex. However with increasing number of [[OpType]] operations and
    * [[Literal]] literals it will grow beyond comprehension...
    */
  def optimize: Expr => Expr = {
    // Integer optimization. Note that the elimination law:
    // i + 0 = i
    // is missing, since it is taken care of by the following clause:
    case Op(Add, Const(IntLit(i1)), Const(IntLit(i2)))  => int(i1 + i2)
    case Op(Eq, Const(IntLit(i1)), Const(IntLit(i2)))   => bool(i1 == i2)
    // Boolean optimization
    case Op(Eq, Const(BoolLit(b1)), Const(BoolLit(b2))) => bool(b1 == b2)
    // Recurse: BOILERPLATE...
    case Op(op, lop, rop)                               =>
      optimize(Op(op, optimize(lop), optimize(rop)))
    case e: Expr                                        => e
  }

  /** Substitute
    *
    * Substitutes [[Expr]] [[Var]] clauses with [[Expr]] from an [[Env]]
    * environment.
    */
  def subst: Env => Expr => Expr = env => {
    case v@Var(name)      => env get name getOrElse v
    // Recurse: BOILERPLATE...
    case Op(op, lop, rop) => Op(op, subst(env)(lop), subst(env)(rop))
    case e: Expr          => e
  }

  /** Evaluate an [[Expr]] expression.
    *
    * eval = optimize . substitute
    *
    * @note however it traverses the given [[Expr]] expression twice.
    */
  def eval: Env => Expr => Boolean = subst(_) andThen optimize andThen {
    case Const(BoolLit(b)) => b
    case _                 => sys.error("Not a boolean !")
  }

  object example {
    val expr: Expr = equal(add(variable("var"))(int(1)))(int(2))
    val env: Env = Map("var" -> int(1))

    val run = eval(env)(expr)
  }
}

object Main extends App {
  println(expr.example.run)
}
