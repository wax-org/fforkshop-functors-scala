package wax.typeclass.applicative.laws.cats

import cats._
import cats.laws.discipline.ApplicativeTests
import cats.tests.CatsSuite
import wax.typeclass.applicative.cats.implicits._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.SemigroupalTests.Isomorphisms.invariant

class ApplicativeSpec extends CatsSuite {
  checkAll("Option.ApplicativeLaws", ApplicativeTests[Option](optionApplicative).applicative[Int, Int, String])
  checkAll("Either.ApplicativeLaws", ApplicativeTests[Either[String, ?]](eitherApplicative[String]).applicative[Int, Int, String])
  checkAll("List.ApplicativeLaws", ApplicativeTests[List](listApplicative).applicative[Int, Int, String])
  checkAll("Tuple.ApplicativeLaws", ApplicativeTests[(String, ?)](tupleApplicative[String]).applicative[Int, Int, String])
  checkAll("Function.ApplicativeLaws", ApplicativeTests[Function[String, ?]](functionApplicative[String]).applicative[Int, Int, String])

  // I don't know how to test function properties in Scala
  // See http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#19
  implicit def stringFunctionEq[A: Eq]: Eq[Function[String, A]] = new Eq[Function[String, A]] {
    override def eqv(x: Function[String, A], y: Function[String, A]): Boolean = x("some-string") == y("some-string")
  }

  implicit def tupleInvariant[C]: Invariant[(C, ?)] = new Invariant[Tuple2[C, ?]] {
    override def imap[A, B](fa: (C, A))(f: A => B)(g: B => A): (C, B) = (fa._1, f(fa._2))
  }

  implicit def tupleIso[C]: Isomorphisms[(C, ?)] = invariant(tupleInvariant)
}
