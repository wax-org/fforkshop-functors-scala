package wax.typeclass.functor.laws

import cats._
import cats.laws.discipline.FunctorTests
import cats.tests.CatsSuite
import wax.typeclass.functor.implicits._

class FunctorSpec extends CatsSuite {
  checkAll("Option.FunctorLaws", FunctorTests[Option](optionFunctor).functor[Int, Int, String])
  checkAll("Either.FunctorLaws", FunctorTests[Either[String, ?]](eitherFunctor[String]).functor[Int, Int, String])
  checkAll("List.FunctorLaws", FunctorTests[List](listFunctor).functor[Int, Int, String])
  checkAll("Tuple.FunctorLaws", FunctorTests[(String, ?)](tupleFunctor[String]).functor[Int, Int, String])
  checkAll("Function.FunctorLaws", FunctorTests[Function[String, ?]](functionFunctor[String]).functor[Int, Int, String])

  // I don't know how to test function properties in Scala
  // See http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#19
  implicit def stringFunctionEq[A: Eq]: Eq[Function[String, A]] = new Eq[Function[String, A]] {
    override def eqv(x: Function[String, A], y: Function[String, A]): Boolean = x("some-string") == y("some-string")
  }
}
