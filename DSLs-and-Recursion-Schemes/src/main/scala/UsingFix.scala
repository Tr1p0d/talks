package using.fix

object fix {
  import cats.Functor
  import cats.syntax.functor._

  final case class Fix[F[_]](unFix: F[Fix[F]]) extends AnyVal

  def unFix[F[_]]: Fix[F] => F[Fix[F]] = _.unFix

  type Algebra[F[_], A] = F[A] => A
  type InitialAlgebra[F[_]] = Algebra[F, Fix[F]]

  def andThen[F[_]](phi: InitialAlgebra[F], psi: InitialAlgebra[F]): InitialAlgebra[F] =
    phi andThen unFix andThen psi

  def andThenAll[F[_]]: List[InitialAlgebra[F]] => InitialAlgebra[F] = {
    lazy val onEmpty = sys.error("andThenAll: empty List")

    _ reduceLeftOption andThen getOrElse(onEmpty)
  }

  def cata[F[_]: Functor, A](
    algebra: Algebra[F, A]
  )(
    fix: Fix[F]
  ): A = algebra(fix.unFix map cata(algebra))
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
  import ExprF._
  import Literal._
  import OpType._
  import cats.Functor
  import cats.implicits._
  import cats.syntax.functor._
  import fix._

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

    implicit val functor: Functor[ExprF] = new Functor[ExprF] {
      def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
        case const: Const[A] => const copy ()
        case variable: Var[A] => variable copy ()
        case Op(opType, opl, opr) => Op(opType, f(opl), f(opr))
      }
    }

    object ConstInt {
      final def unapply(c: Expr): Option[Int] = c match {
        case Fix(Const(IntLit(i))) => Some(i)
        case _ => None
      }
    }

    object ConstBool {
      final def unapply(c: Expr): Option[Boolean] = c match {
        case Fix(Const(BoolLit(b))) => Some(b)
        case _ => None
      }
    }
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
    case Op(Eq, ConstInt(i1), ConstInt(i2)) => bool(i1 == i2)
    case e: ExprF[Expr] => Fix(e)
  }

  /** An [[InitialAlgebra]] optimizing (Int, +).
    */
  val optimizeIntAddA: InitialAlgebra[ExprF] = {
    case Op(Add, ConstInt(i1), ConstInt(i2)) =>
      int(i1 + i2)
    case e: ExprF[Expr] => Fix(e)
  }

  /** An [[InitialAlgebra]] optimizing (Boolean, ==).
    */
  val optimizeBoolEqA: InitialAlgebra[ExprF] = {
    case Op(Eq, ConstBool(b1), ConstBool(b2)) => bool(b1 == b2)
    case e: ExprF[Expr] => Fix(e)
  }

  /** Optimizer
    *
    * Expressed as a composition of [[InitialAlgebra]]s.
    */
  val optimizeA: InitialAlgebra[ExprF] = andThenAll(
    List(optimizeIntEqA, optimizeIntAddA, optimizeBoolEqA)
  )

  /** Substitute
    *
    * Substitutes [[Expr]] [[Var]] clauses with [[Expr]] from an [[Env]]
    * environment. Expressed in terms of [[InitialAlgebra]].
    */
  val substA: Env => InitialAlgebra[ExprF] = env => {
    case v@Var(name)    => env get name getOrElse Fix(v)
    case e: ExprF[Expr] => Fix(e)
  }

  val evalA: Env => InitialAlgebra[ExprF] = env => andThen(substA(env), optimizeA)

  val eval: Env => Expr => Expr = env => cata(evalA(env))

  object example {
    val expr: Expr = equal(add(variable("var"))(int(1)))(int(2))
    val env: Env = Map("var" -> int(1))

    val run = eval(env)(expr)
  }
}

object Main extends App {
  println(expr.example.run)
}
