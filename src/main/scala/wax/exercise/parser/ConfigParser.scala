package wax.exercise.parser

import cats.implicits._
import cats.{Applicative, Functor, SemigroupK}

import scala.language.higherKinds

object ConfigParser {
  import Parser._

  def parseConfig(cfg: String): Config = config.unsafeRun(cfg)

  val config: Parser[Config] = ???
}

sealed trait ParserResult[A]
case class ParserSuccess[A](value: A, remainder: String) extends ParserResult[A]
case class ParserFailure[A]() extends ParserResult[A]

case class Parser[A](parse: String => ParserResult[A]) {
  def run(input: String): Either[String, A] = parse(input) match {
    case ParserFailure()                   => Left("parser error")
    case ParserSuccess(_, r) if r.nonEmpty => Left("parser did not consume entire stream: '" ++ r ++ "'")
    case ParserSuccess(v, _)               => Right(v)
  }

  def unsafeRun(input: String): A = run(input).left.map(e =>
    throw new RuntimeException(s"could not parse input:\n$input\n\n$e")
  ).toTry.get
}

object Parser {
  implicit val parserResultFunctor: Functor[ParserResult] = new Functor[ParserResult] {
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = ???
  }

  implicit val parserResultApplicative: Applicative[ParserResult] = new Applicative[ParserResult] {
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = parserResultFunctor.map(fa)(f)

    override def pure[A](x: A): ParserResult[A] = ???

    override def ap[A, B](ff: ParserResult[A => B])(fa: ParserResult[A]): ParserResult[B] = ???
  }

  implicit val parserFunctor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = ???
  }

  implicit val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    override def pure[A](x: A): Parser[A] = ???

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = ???
  }

  implicit lazy val parserSemigroupK: SemigroupK[Parser] = new SemigroupK[Parser] {
    // x <+> y returns first successful parser
    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = ???
  }

  def satisfy(pred: Char => Boolean): Parser[Char] = Parser { s =>
    if (s.nonEmpty && pred(s.head)) ParserSuccess(s.head, s.tail)
    else ParserFailure()
  }

  def char(a: Char): Parser[Char] = satisfy(_ == a)

  def notChar(a: Char): Parser[Char] = satisfy(_ != a)

  def anyChar: Parser[Char] = satisfy(_ => true)

  def space: Parser[Char] = char(' ')

  def digit: Parser[Char] = ???

  def letter: Parser[Char] = ???

  def letterOrDigit: Parser[Char] = ???

  def int: Parser[Int] = ???

  def string(str: String): Parser[String] = ???

  def alphaString: Parser[String] = many(letter).map(_.mkString)

  def alphaNumString: Parser[String] = many(letterOrDigit).map(_.mkString)

  def tokenize[A](parser: Parser[A]): Parser[A] =
    many(space) *> parser <* many(space)

  def many[A](parser: Parser[A]): Parser[List[A]] = Parser(s =>
    parser.parse(s) match {
      case ParserFailure() => ParserSuccess(List.empty, s)
      case ParserSuccess(v, s1) => many(parser).parse(s1).fmap(xs => v :: xs)
    }
  )

  def some[A](parser: Parser[A]): Parser[List[A]] = Parser(s =>
    parser.parse(s) match {
      case ParserFailure() => ParserFailure()
      case ParserSuccess(v, s1) => many(parser).parse(s1).fmap(xs => v :: xs)
    }
  )

  def skipMany[A](parser: Parser[A]): Parser[Unit] = Parser(s =>
    parser.parse(s) match {
      case ParserFailure() => ParserSuccess(Unit, s)
      case ParserSuccess(_, s1) => skipMany(parser).parse(s1)
    }
  )
}
