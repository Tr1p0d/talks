import $ivy.`org.typelevel::cats-core:1.6.0`

import java.time.{Duration, ZonedDateTime}
import cats.{Contravariant, ContravariantMonoidal, Functor}
import cats.implicits._

trait Diff[In] {
  type Out

  def apply(in1: In, in2: In): Out
}

final case class Bar(string: String)
final case class Quux(bool: Boolean)
final case class Foo(int: Int, zdt: ZonedDateTime, bar: Bar, quux: Quux)

final case class FooDiff(intDiff: Int, zdtDiff: Duration, barDiff: String)

object Diff {

  trait ContravariantDiff extends Contravariant[Diff] {
    def contramap[A, B](fa: Diff[A])(f: B => A): Diff[B] = new Diff[B] {
      type Out = fa.Out

      def apply(in1: B, in2: B): Out = fa(f(in1), f(in2))
    }
  }

  implicit val contravariantDiff: Contravariant[Diff] = new ContravariantDiff {}

  trait ContravariantMonoidalDiff 
    extends ContravariantDiff
    with ContravariantMonoidal[Diff] {

    def unit: Diff[Unit] = new Diff[Unit] {
      type Out = Unit

      def apply(in1: Unit, in2: Unit): Unit = ()
    }

    def product[A, B](fa: Diff[A], fb: Diff[B]): Diff[(A, B)] = new Diff[(A, B)] {
      type Out = (fa.Out, fb.Out)

      def apply(in1: (A, B), in2: (A, B)): Out = 
        ( fa(in1._1, in2._1)
        , fb(in1._2, in2._2)
        )
    }
  }

  implicit def contravariantMonoidalDiff: ContravariantMonoidal[Diff] = 
    new ContravariantMonoidalDiff {}

  def apply[A: Diff]: Diff[A] = implicitly

  implicit val intDiff = new Diff[Int] {
    type Out = Int

    override def apply(in1: Int, in2: Int): Out = in1 - in2
  }

  implicit val stringDiff = new Diff[String] {
    type Out = Boolean

    override def apply(in1: String, in2: String): Out = in1 == in2
  }

  implicit val barDiff: Diff[Bar] = Diff[String].contramap(_.toString)

  implicit val booleanDiff: Diff[Boolean] = new Diff[Boolean] {
    type Out = Boolean

    override def apply(in1: Boolean, in2: Boolean): Out = in1 && in2
  }

  implicit val quuxDiff: Diff[Quux] = Diff[Boolean].contramap(_.bool)

  implicit val zdtDiff = new Diff[ZonedDateTime] {
    type Out = Duration

    override def apply(in1: ZonedDateTime, in2: ZonedDateTime): Out = 
      Duration.between(in1, in2)
  }
}

object DiffSyntax {
  implicit class DiffOps[In](val in1: In) extends AnyVal {
    def diff1[Out](in2: In)(implicit diff: Diff[In]): diff.Out = diff(in1, in2)
  }
}

object Example {
  import Diff._
  import DiffSyntax._

  def fooDiffer: Diff[Foo] = 
    ( Diff[Int]
    , Diff[ZonedDateTime]
    , Diff[Bar]
    , Diff[Quux]
    ) contramapN (foo => Foo.unapply(foo).get)
  
  def diffFoos(in1: Foo, in2: Foo): Diff[Foo]#Out = fooDiffer(in1, in2)

}

// println(1D.diff1(3D))
// println("12341234".diff1("123412"))
// println(1234 diff1 1235)

val foo1 = Foo(1, ZonedDateTime.now, Bar("2"), Quux(true))
val foo2 = Foo(2, ZonedDateTime.now, Bar("1"), Quux(false))

println(Example.diffFoos(foo1, foo2))
