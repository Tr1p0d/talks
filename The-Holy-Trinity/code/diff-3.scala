import $ivy.`org.typelevel::cats-core:1.6.0`
import $ivy.`com.chuusai::shapeless:2.3.3`

import cats.{Contravariant, ContravariantMonoidal}
import cats.implicits._
import java.util.UUID
import shapeless._
import shapeless.syntax.std.tuple._

trait Diff[In] {
  type Out

  def apply(in1: In, in2: In): Out
}

object Diff {
  type Aux[In, Out0] = Diff[In] { type Out = Out0 }

  trait ContravariantDiff extends Contravariant[Diff] {
    def contramap[A, B](
      fa: Diff[A]
    )(
      f: B => A
    ): Diff.Aux[B, fa.Out] = new Diff[B] {
      type Out = fa.Out

      def apply(in1: B, in2: B): Out = fa.apply(f(in1), f(in2))
    }
  }

  implicit val contravariantDiff: Contravariant[Diff] = 
    new ContravariantDiff {}

  trait ContravariantMonoidalDiff 
    extends ContravariantDiff
    with ContravariantMonoidal[Diff] {

      //def pure: A => Diff[A]
      def unit: Diff.Aux[Unit, Unit] = new Diff[Unit] {
        type Out = Unit

        def apply(in1: Unit, in2: Unit): Out = ()
      }

      //def ap[A, B](f: F[A => B])(fa: F[A]): F[B]
      def product[A, B](
        fa: Diff[A], 
        fb: Diff[B]
      ): Diff.Aux[(A, B), (fa.Out, fb.Out)] = new Diff[(A, B)] {
        type Out = (fa.Out, fb.Out)

        def apply(in1: (A, B), in2: (A, B)): Out = 
          ( fa(in1._1, in2._1)
          , fb(in1._2, in2._2)
          )
      }
  }

  implicit val contravariantMonoidalDiff: ContravariantMonoidal[Diff] =
    new ContravariantMonoidalDiff {}

  val intD: Diff.Aux[Int, Int] = new Diff[Int] {
    type Out = Int

    def apply(in1: Int, in2: Int): Int = in1 - in2
  }

  val stringD: Diff[String] = new Diff[String] {
    type Out = Boolean

    def apply(in1: String, in2: String): Boolean = in1 == in2
  }

  val uuidD: Diff[UUID] = {
    stringD contramap { uuid: UUID => uuid.toString }
  }
}

import Diff._

final case class Foo(int: Int, string: String, id: UUID)
type FooDiff = (Int, (Boolean, Boolean))

val uuid = UUID.randomUUID

val fooD: Diff[Foo]  = 
  ( intD
  , stringD
  , uuidD
  ) contramapN ( Foo.unapply(_).get )

val foo1 = Foo(1, "2", uuid)
val foo2 = Foo(1, "2", uuid)

def diffs[A, B](
  in1: A,
  in2: A,
  diff: Diff[A]
)(
  implicit typeEq: diff.Out =:= B  
): B = diff(in1, in2)

val strDiff: Boolean = stringD("1234", "1234")

println(diffs[String, Boolean]("1234", "1234", stringD))
