package wax.exercise.parser

import cats.{Applicative, Functor}
import cats.data.NonEmptyList
import cats.kernel.Semigroup

import scala.language.postfixOps

object ConfigValidator {

  sealed trait Validated[+E, +A]
  case class Valid[+A](a: A) extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  case class ConfigValidationError(field: String, message: String)

  implicit def validatedFunctor[E]: Functor[Validated[E, ?]] = new Functor[Validated[E, ?]] {
    override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = ???
  }

  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] = new Applicative[Validated[E, ?]] {
    override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = validatedFunctor.map(fa)(f)

    override def pure[A](x: A): Validated[E, A] = ???

    override def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] = ???
  }

  def validateConfig(config: Config): Validated[NonEmptyList[ConfigValidationError], Config] = {
    import CommonValidators._

    ???
  }
}

object CommonValidators {
  import ConfigValidator._

  def nonEmpty(field: String, value: String): Validated[NonEmptyList[ConfigValidationError], String] =
    if (value.length > 0) Valid(value)
    else Invalid(NonEmptyList.one(ConfigValidationError(field, "value must not be empty")))

  def validatePort(field: String, p: Int): Validated[NonEmptyList[ConfigValidationError], Int] =
    if (p >= 0 && p <= 65536) Valid(p)
    else Invalid(NonEmptyList.one(ConfigValidationError(field, "port must be an int between 0 and 65536")))

  def validateHost(field: String, h: String): Validated[NonEmptyList[ConfigValidationError], String] =
    if (h.matches("[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+")) Valid(h)
    else Invalid(NonEmptyList.one(ConfigValidationError(field, "host must be a proper hostname/ip without port")))
}
