package wax.exercise.parser

import cats._
import cats.implicits._
import cats.laws.discipline._
import cats.tests.CatsSuite
import org.scalacheck._
import wax.exercise.parser.Parser._
import wax.exercise.parser.ParserResultImplicits._
import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.enablers.CheckerAsserting

class ParserResultFunctorSpec extends CatsSuite {
  checkAll("ParserResult", FunctorTests[ParserResult].functor[Int, Int, String])
}

class ParserResultApplicativeSpec extends ParserResultFunctorSpec {
  checkAll("ParserResult", ApplicativeTests[ParserResult].applicative[Int, Int, String])

  // These tests check that your Applicative instance not only lawful, but the one that we need.
  test("reminders are combined in correct order") {
    forAll { (r1: AdequateString, r2: AdequateString) =>
      val res1: ParserResult[Int => Int] = ParserSuccess(x => x + 1, r1.value)
      val res2: ParserResult[Int] = ParserSuccess(42, r2.value)
      assert(res1 <*> res2 == ParserSuccess(43, r1.value + r2.value))
    }
  }
}

object ParserResultImplicits {
  implicit def parserResultEq[A]: Eq[ParserResult[A]] = new Eq[ParserResult[A]] {
    override def eqv(x: ParserResult[A], y: ParserResult[A]): Boolean = (x, y) match {
      case (ParserFailure(), ParserFailure())             => true
      case (ParserSuccess(s1, v1), ParserSuccess(s2, v2)) => s1 == s2 && v1 == v2
      case _                                              => false
    }
  }

  implicit def arbParserResult[A: Arbitrary]: Arbitrary[ParserResult[A]] =
    Arbitrary(Gen.oneOf(
      Gen.const(ParserFailure[A]()),
      for {
        s <- Arbitrary.arbitrary[String]
        v <- Arbitrary.arbitrary[A]
      } yield ParserSuccess[A](v, s)
    ))
}
