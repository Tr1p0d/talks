import java.time.{Duration, ZonedDateTime}

import scala.collection.immutable.Map

final case class Bar(string: String)
final case class Foo(int: Int, zdt: ZonedDateTime, bar: Bar)

def diffFoos(foo1: Foo, foo2: Foo): Map[String, String] = {
  var result: Map[String, String] = Map.empty

  result + ("Int" -> (foo1.int - foo2.int).toString)
  result += ("Zoned Date Time" -> Duration.between(foo1.zdt, foo2.zdt).toString)
  result += ("Bar" -> (foo1.bar.string == foo2.bar.string).toString)

  result
}

val foo1 = Foo(1, ZonedDateTime.now(), Bar("bar1"))
val foo2 = Foo(2, ZonedDateTime.now().plusHours(1), Bar("bar2"))

println(diffFoos(foo1, foo2))
