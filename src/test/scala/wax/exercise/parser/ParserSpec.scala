package wax.exercise.parser

import cats.{Applicative, Eq}
import cats.implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import wax.exercise.parser.Parser._
import ParserSpecUtils._

object ParserFunctorSpec extends Properties("Parser.Functor") {
  // it's hard to test this property in general, so let's test for a less general case.
  property("fmap id = id") = forAll { input: AdequateString =>
    checkResult(someParser.fmap(identity[String]), input, input.value)
  }
}

object ParserApplicativeSpec extends Properties("Parser.Applicative") {
  // it's hard to test this property in general, so let's test for a less general case.
  property("pure id <*> v = v") = forAll { (x: AdequateString, input: AdequateString) =>
    val v = Applicative[Parser].pure(x.value)
    val p1 = Applicative[Parser].pure[String => String](identity[String]).ap(v)
    val p2 = v
    compareParsers(p1, p2)(input)
  }

  property("pure f <*> pure x = pure (f x)") = forAll { (x: AdequateString, input: AdequateString) =>
    val f: String => Int = s => s.length
    val p1: Parser[Int]  = Applicative[Parser].pure[String => Int](f).ap(Applicative[Parser].pure[String](x.value))
    val p2: Parser[Int]  = Applicative[Parser].pure[Int](f(x.value))
    compareParsers(p1, p2)(input)
  }

  property("u <*> pure y = pure ($ y) <*> u") = forAll { (y: AdequateString, input: AdequateString) =>
    val f: String => Int = s => s.length
    val u                = Applicative[Parser].pure[String => Int](f)
    val p1: Parser[Int]  = u.ap(Applicative[Parser].pure[String](y.value))
    val p2: Parser[Int]  = Applicative[Parser].pure[(String => Int) => Int](f => f(y.value)).ap(u)
    compareParsers(p1, p2)(input)
  }

  // pure (.) <*> u <*> v <*> w = u <*> (v <*> w) is omitted

  property("fmap f x = pure f <*> x") = forAll { (v: AdequateString, input: AdequateString) =>
    val f: String => Int = s => s.length
    val x: Parser[String] = Applicative[Parser].pure[String](v.value)
    val p1: Parser[Int] = x.fmap(f)
    val p2: Parser[Int] = Applicative[Parser].pure[String => Int](f).ap(x)
    compareParsers(p1, p2)(input)
  }

  // These tests check that your Applicative instance not only lawful, but the one that we need.
  property("input is not thrown away by pure") = forAll { (x: AdequateString, input: AdequateString) =>
    checkResult(Applicative[Parser].pure[String](x.value), input.value, x.value, input.value)
  }

  property("input is not duplicated by ap") = forAll { (x: Char, y: Char) =>
    val input = s"$x$y"
    val p1: Parser[Char] = char(x)
    val p2: Parser[Char] = char(y)
    checkResult(p1.map((a: Char) => (b: Char) => s"$a$b").ap(p2), input, input, "")
  }
}

object ParserBasicSpec extends Properties("Parser.Basic") {
  property("char parser consumes only the first character of the string") = forAll { input: AdequateString =>
    val s = input.value
    if (s.isEmpty) Parser.char(' ').parse(s) == ParserFailure()
    else checkResult(Parser.char(s.head), s, s.head, s.tail)
  }

  property("notChar parser consumes any character from the string except for specific") = forAll {
    (c: Char, input: AdequateString) =>
      val s = input.value
      if (s.isEmpty) Parser.notChar(' ').parse(s) == ParserFailure()
      else if (s.head == c) Parser.notChar(c).parse(s) == ParserFailure()
      else checkResult(Parser.notChar(c), s, s.head, s.tail)
  }

  property("anyChar parser consumes any character from the string") = forAll { input: AdequateString =>
    val s = input.value
    if (s.isEmpty) Parser.anyChar.parse(s) == ParserFailure()
    else checkResult(Parser.anyChar, s, s.head, s.tail)
  }
}

object ParserDigitSpec extends Properties("Parser.Digit") {
  property("digit parser consumes digit character from the string") = forAll {
    (digitChar: DigitChar, nonDigitChar: NonDigitChar) =>
      Parser.digit.parse(s"$digitChar$nonDigitChar") == ParserSuccess(digitChar, s"$nonDigitChar")
      Parser.digit.parse(s"$nonDigitChar$digitChar") == ParserFailure()
  }
}

object ParserNatSpec extends Properties("Parser.Nat") {
  property("natural number parser consumes positive integers") = forAll { v: Nat =>
    checkResult(Parser.nat, v.toString, v.value)
  }
}

object ParserIntSpec extends Properties("Parser.Int") {
  property("int parser consumes any int (positive and negative)") = forAll { v: Short =>
    checkResult(Parser.int, v.toString, v.toInt)
  }
}

object ParserStringSpec extends Properties("Parser.String") {
  property("parser(string(s)).parse(s) == s.pure") = forAll { input: AdequateString =>
    val s = input.value
    Parser.string(s).parse(s) == s.pure[ParserResult]
  }

  property("string doesn't eat all the input") = forAll { input: AdequateString =>
    val s = input.value
    Parser.string(s).parse(s ++ "---boris") == ParserSuccess(s, "---boris")
  }
}

object ParserTokenSpec extends Properties("Parser.Token") {
  property("token parser ignores spaces around") = forAll { (v: Int, spacesBefore: Int, spacesAfter: Int) =>
    val s = (" " * (spacesBefore.abs % 20)) ++ v.toString ++ (" " * (spacesAfter.abs % 20))
    Parser.tokenize(Parser.int).parse(s) == ParserSuccess(v, "")
  }
}

object ParserSpecUtils {
  val someParser: Parser[String] = Parser(a => ParserSuccess(a, ""))

  def checkResult[A: Eq](parser: Parser[A], input: AdequateString, expected: A): Boolean =
    checkResult(parser, input.value, expected)

  def checkResult[A: Eq](parser: Parser[A], input: String, expected: A, remainder: String = ""): Boolean = {
    val r = parser.parse(input)
    val v = ParserSuccess(expected, remainder)
    val equal = r == v
    if (!equal) {
      println("")
      println("Test failure!")
      println(s"Input: '$input'")
      println(s"Expected: $v")
      println(s"Result:   ${parser.parse(input)}")
    }
    equal
  }

  def compareParsers[A: Eq](p1: Parser[A], p2: Parser[A])(input: AdequateString): Boolean = {
    val r1 = p1.parse(input.value)
    val r2 = p2.parse(input.value)
    val equal = r1 == r2
    if (!equal) {
      println("")
      println("Test failure!")
      println(s"Input: '$input'")
      println(s"Result: $r1 != $r2")
    }
    equal
  }
}
