package wax.exercise.parser

import cats.data.NonEmptyList
import cats.laws.discipline.ApplicativeTests
import cats.tests.CatsSuite
import cats.{Applicative, Eq}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, _}
import wax.exercise.parser.ConfigValidator._

class ValidatedSpec extends CatsSuite {
  implicit val validatedApplicative: Applicative[Validated[NonEmptyList[Int], ?]] = ConfigValidator.validatedApplicative[NonEmptyList[Int]]

  implicit val eq: Eq[Validated[NonEmptyList[Int], Int]] = _ == _
  implicit val eq2: Eq[Validated[NonEmptyList[Int], (Int, Int, Int)]] = _ == _
  implicit def arbitraryNel[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Arbitrary {
    arbitrary[List[A]] filter (_.nonEmpty) map (l => NonEmptyList.fromListUnsafe(l))
  }
  implicit def arbitraryValid[A: Arbitrary]: Arbitrary[Valid[A]] = Arbitrary { arbitrary[A] map Valid.apply }
  implicit def arbitraryInvalid[A: Arbitrary]: Arbitrary[Invalid[A]] = Arbitrary { arbitrary[A] map Invalid.apply }

  checkAll(
    "ValidatedNel",
    {
      implicit val arbitraryValidated: Arbitrary[Validated[NonEmptyList[Int], Int]] = Arbitrary {
        arbitrary[Boolean].flatMap {
          case true => arbitraryValid[Int].arbitrary
          case false => arbitraryInvalid[NonEmptyList[Int]].arbitrary
        }
      }

      implicit val arbitraryValidatedF: Arbitrary[Validated[NonEmptyList[Int], Int => Int]] = Arbitrary {
        arbitrary[Int => Int] map Valid.apply
      }

      ApplicativeTests[Validated[NonEmptyList[Int], ?]](validatedApplicative).applicative[Int, Int, Int]
    }
  )

  test("ValidatedNel error aggregation") {
    Prop.forAll { (invalid1: Invalid[NonEmptyList[Int]], invalid2: Invalid[NonEmptyList[Int]]) =>
      val res = validatedApplicative.fmap(invalid1)(identity).ap(invalid2)
      res match {
        case Invalid(errors) => errors == invalid1.e ::: invalid2.e
        case _ => false
      }
    }.check
  }
}
