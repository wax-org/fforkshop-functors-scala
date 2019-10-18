package wax.exercise.parser

import cats._
import cats.tests.CatsSuite
import cats.laws.discipline._
import wax.exercise.parser.Parser._
import org.scalacheck._

class ParserResultSpec extends CatsSuite {
  checkAll("ParserResult", FunctorTests[ParserResult].functor[Int, Int, String])
  checkAll("ParserResult", ApplicativeTests[ParserResult].applicative[Int, Int, String])

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
