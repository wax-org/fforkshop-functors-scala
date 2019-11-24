package wax.exercise.parser

import cats.implicits._
import cats.{Applicative, Functor, SemigroupK}

import scala.language.higherKinds

/*

Welcome, traveller!

Your goal is to add missing implementations in this file.

Please do read the comments as they will guide you along the path.

 */

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


  /** Task 1. Implement Functor instance for ParserResult.
   *
   * Run wax.exercise.parser.ParserResultFunctorSpec and make sure that it's green.
   */
  implicit val parserResultFunctor: Functor[ParserResult] = new Functor[ParserResult] {
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = ???
  }

  /** Task 2. Implement Functor instance for Parser.
   *
   * Run wax.exercise.parser.ParserFunctorSpec and make sure that it's green.
   */
  implicit val parserFunctor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = ???
  }

  /** Task 3. Implement Applicative instance for ParserResult.
   *
   * Run wax.exercise.parser.ParserResultApplicativeSpec and make sure that it's green. Note that it also relies on
   * the proper implementation of Functor instance.
   */
  implicit val parserResultApplicative: Applicative[ParserResult] = new Applicative[ParserResult] {
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = parserResultFunctor.map(fa)(f)

    override def pure[A](x: A): ParserResult[A] = ???

    override def ap[A, B](ff: ParserResult[A => B])(fa: ParserResult[A]): ParserResult[B] = ???
  }

  /** Task 4. Implement Applicative instance for ParserResult.
   *
   * Run wax.exercise.parser.ParserApplicativeSpec and make sure that it's green. Note that it also relies on
   * the proper implementation of Functor instance.
   *
   * There are multiple valid (lawful) implementations of this Parser, but we need specific one. Tests will guide you.
   */
  implicit val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = parserFunctor.map(fa)(f)

    override def pure[A](x: A): Parser[A] = ???

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = ???
  }

  def satisfy(pred: Char => Boolean): Parser[Char] = Parser { s =>
    if (s.nonEmpty && pred(s.head)) ParserSuccess(s.head, s.tail)
    else ParserFailure()
  }

  def char(a: Char): Parser[Char] = satisfy(_ == a)

  def notChar(a: Char): Parser[Char] = satisfy(_ != a)

  def anyChar: Parser[Char] = satisfy(_ => true)

  def space: Parser[Char] = char(' ')

  /** Task 5. Implement a parser for digit char.
   *
   * For example, digit.parse("a") should fail
   *              digit.parse("1") should succeed
   *
   * Run wax.exercise.parser.ParserDigitSpec and make sure that it's green.
   */
  def digit: Parser[Char] = ???

  def letter: Parser[Char] = satisfy(_.isLetter)

  def letterOrDigit: Parser[Char] = satisfy(_.isLetterOrDigit)

  /** Task 6. Implement parser for positive integers.
   *
   * Remember that you have the following combinators at your disposal:
   *   - many[A] - that repeats parser zero or more times and returns the list of parsed values
   *   - digit - that parser one digit char
   *
   * Also you have string.toInt function :)
   *
   * Run wax.exercise.parser.ParserNatSpec and make sure that it's green.
   */
  def nat: Parser[Int] = ???

  implicit lazy val parserSemigroupK: SemigroupK[Parser] = new SemigroupK[Parser] {
    // x <+> y returns first successful parser
    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = Parser { s =>
      x.parse(s) match {
        case ParserFailure() => y.parse(s)
        case r => r
      }
    }
  }

  /** Task 7. Implement parser for positive integers.
   *
   * Remember that you have the following combinators at your disposal:
   *   - many[A] - that repeats parser zero or more times and returns the list of parsed values
   *   - digit - that parser one digit char
   *   - char - that parses specified character
   *   - parser1 <+> parser2 - that returns result of the first successful parser
   *
   * Also you have string.toInt function :) And you can reuse nat parser.
   *
   * Also, remember that you can do (parser1, parser2).mapN((result1, result2) => doSomething)
   *
   * Run wax.exercise.parser.ParserIntSpec and make sure that it's green.
   */
  def int: Parser[Int] = ???

  /** Task 8. Implement parser specific string.
   *
   * Example:
   *
   *   - string("hello").parse("hello world") returns ParserSuccess("hello", " world")
   *   - string("hello").parse("boris") returns ParserFailure()
   *
   * Remember that you have the following helpers at your disposal:
   *   - char parser
   *   - recursion
   *   - fmap/map from Functor
   *   - pure from Applicative
   *   - ap from Applicative
   *   - *> from Applicative (runs the parser, but discards result)
   *
   * You don't need to use all of that, everything depends on your solution.
   *
   * Run wax.exercise.parser.ParserStringSpec and make sure that it's green.
   */
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

object ConfigParser {
  import Parser._

  def parseConfig(cfg: String): Config = config.unsafeRun(cfg)

  /** Task 9*. Implement config parser.
   *
   * Run wax.exercise.parser.ConfigParserSpec and make sure that it's green.
   *
   * Remember that you have the following helpers at your disposal:
   *   - int parser combinator
   *   - string parser combinator
   *   - many parser combinator
   *   - letter/letterOrDigit parser combinator
   *   - tokenize parser combinator that ignores spaces
   *   - fmap/map from Functor
   *   - pure from Applicative
   *   - ap from Applicative
   *   - *> from Applicative (runs the parser, but discards result)
   *   - mapN - this is your friend in Scala!
   *
   * You don't need to use all of that, everything depends on your solution.
   */
  val config: Parser[Config] = ???
}
