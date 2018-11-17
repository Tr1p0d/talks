package using.fix

object fix {
  import cats.Foldable
  import cats.syntax.foldable._

  final case class Fix[F[_]](unFix: F[Fix[F]]) extends AnyVal

  def unFix[F[_]]: Fix[F] => F[Fix[F]] = _.unFix

  type Algebra[F[_], A] = F[A] => A
  type InitialAlgebra[F[_]] = Algebra[F, Fix[F]]

  def andThen[F[_]](phi: InitialAlgebra[F], psi: InitialAlgebra[F]): InitialAlgebra[F] =
    phi andThen unFix andThen psi

  def andThenAll[F[_]]: List[InitialAlgebra[F]] => InitialAlgebra[F] = {
    val onEmpty = sys.error("andThenAll: empty Foldable")

    _ reduceLeftOption andThen getOrElse(onEmpty)
  }
}

object intList {
  import fix._
  import IntListF._

  sealed trait IntListF[A]

  type FixIntList = Fix[IntListF]

  object IntListF {
    final case class IntConsF[A](i: Int, is: A) extends IntListF[A]
    final case class IntNilF[A]()               extends IntListF[A]
  }

  def intNil: FixIntList = Fix(IntNilF())
  def intCons(i: Int, is: FixIntList): FixIntList = Fix(IntConsF(i, is))

  val intList: FixIntList = intCons(3, intCons(2, intCons(1, intNil)))

  val sumAlgebra: Algebra[IntListF, Int] = {
    case IntConsF(i, is) => i + is
    case IntNilF()       => 0
  }

  val printAlgebra: Algebra[IntListF, String] = {
    case IntConsF(i, is) => s"[$i $is]"
    case IntNilF()       => ""
  }

  val addOneAlgebra: InitialAlgebra[IntListF] = {
    case IntConsF(i, is) => Fix(IntConsF(i + 1, is))
    case IntNilF()       => Fix(IntNilF())
  }
}

object list {
  import fix._
  import FixListF._

  sealed trait FixListF[A, B]

  type FixList[A] = Fix[FixListF[A, ?]]

  object FixListF {
    final case class FixNilF[A, B]()            extends FixListF[A, B]
    final case class FixConsF[A, B](a: A, b: B) extends FixListF[A, B]
  }

  def nil[A]: FixList[A] = Fix[FixListF[A, ?]](FixNilF())
  def cons[A](a: A, as: FixList[A]): FixList[A] = Fix[FixListF[A, ?]](FixConsF(a, as))

  val intList: FixList[Int] = cons(3, cons(2, cons(1, nil)))
}

object expr {
  import cats.implicits._
  import fix._
  import Literal._
  import OpType._
  import ExprF._

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

  sealed trait ExprF[A]

  object ExprF {
    final case class Op[A](op: OpType, opl: A, opr: A) extends ExprF[A]
    final case class Const[A](lit: Literal) extends ExprF[A]
    final case class Var[A](name: Name) extends ExprF[A]
  }

  type Expr = Fix[ExprF]

  val int: Int => Expr = i => Fix(Const(IntLit(i)))
  val bool: Boolean => Expr = b => Fix(Const(BoolLit(b)))
  val variable: Name => Expr = n => Fix(Var(n))
  val equal: Expr => Expr => Expr = lop => rop => Fix(Op(Eq, lop, rop))
  val add: Expr => Expr => Expr = lop => rop => Fix(Op(Add, lop, rop))

  /** An [[InitialAlgebra]] optimizing (Int, ==).
    */
  val optimizeIntEqA: InitialAlgebra[ExprF] = {
    case Op(Eq, Fix(Const(IntLit(i1))), Fix(Const(IntLit(i2)))) => bool(i1 == i2)
    case e: ExprF[Expr] => Fix(e)
  }

  /** An [[InitialAlgebra]] optimizing (Int, +).
    */
  val optimizeIntAddA: InitialAlgebra[ExprF] = {
    case Op(Eq, Fix(Const(IntLit(i1))), Fix(Const(IntLit(i2)))) => int(i1 + i2)
    case e: ExprF[Expr] => Fix(e)
  }

  /** An [[InitialAlgebra]] optimizing (Boolean, ==).
    */
  val optimizeBoolEqA: InitialAlgebra[ExprF] = {
    case Op(Eq, Fix(Const(BoolLit(b1))), Fix(Const(BoolLit(b2)))) => bool(b1 == b2)
    case e: ExprF[Expr] => Fix(e)
  }

  /** Optimizer
    *
    * Expressed as a composition of [[InitialAlgebra]]s.
    */
  val optimize: InitialAlgebra[ExprF] = andThenAll(
    List(optimizeIntEqA, optimizeIntAddA, optimizeBoolEqA)
  )

  /** Substitute
    *
    * Substitutes [[Expr]] [[Var]] clauses with [[Expr]] from an [[Env]]
    * environment. Expressed in terms of [[InitialAlgebra]].
    */
  def subst: Env => InitialAlgebra[ExprF] = env => {
    case v@Var(name)    => env get name getOrElse Fix(v)
    case e: ExprF[Expr] => Fix(e)
  }

  def eval: Env => Expr => Boolean = env => {
    val eval1: InitialAlgebra[ExprF] = andThen(subst(env), optimize)

    ??? //TODO
  }
}

object Main extends App {
}
