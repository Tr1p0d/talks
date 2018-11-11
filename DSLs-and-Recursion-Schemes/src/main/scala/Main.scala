object fix {
  final case class Fix[F[_]](unFix: F[Fix[F]]) extends AnyVal

  type Algebra[F[_], A] = F[A] => A
  type InitialAlgebra[F[_]] = Algebra[F, Fix[F]]
}

object a {
  import IntList._

  sealed trait IntList

  object IntList {
    final case class IntCons(i: Int, is: IntList) extends IntList
    final case object IntNil                     extends IntList
  }

  def intNil: IntList = IntNil
  def intCons(i: Int, is: IntList): IntList = IntCons(i, is)

  val intList: IntList = intCons(3, intCons(2, intCons(1, intNil)))
}

object b {
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

}
