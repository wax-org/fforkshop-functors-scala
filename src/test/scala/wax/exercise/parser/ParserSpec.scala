package wax.exercise.parser

import cats.implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import wax.exercise.parser.Parser._

object ParserSpec extends Properties("Parser") {
  property("char parser consumes only the first character of the string") = forAll { s: String =>
    if (s.isEmpty) Parser.char(' ').parse(s) == ParserFailure()
    else Parser.char(s.head).parse(s) == ParserSuccess(s.head, s.tail)
  }

  property("notChar parser consumes any character from the string except for specific") = forAll { (c: Char, s: String) =>
    if (s.isEmpty) Parser.notChar(' ').parse(s) == ParserFailure()
    else if (s.head == c) Parser.notChar(c).parse(s) == ParserFailure()
    else Parser.notChar(c).parse(s) == ParserSuccess(s.head, s.tail)
  }

  property("anyChar parser consumes any character from the string") = forAll { s: String =>
    if (s.isEmpty) Parser.anyChar.parse(s) == ParserFailure()
    else Parser.anyChar.parse(s) == ParserSuccess(s.head, s.tail)
  }

  property("parser(string(s)).parse(s) == s.pure") = forAll { s: String =>
    Parser.string(s).parse(s) == s.pure[ParserResult]
  }

  property("number parser consumes number") = forAll { v: Int =>
    Parser.int.parse(v.toString) == ParserSuccess(v, "")
  }

  property("token parser ignores spaces around") = forAll { (v: Int, spacesBefore: Int, spacesAfter: Int ) =>
    val s = (" " * (spacesBefore.abs % 20)) ++ v.toString ++ (" " * (spacesAfter.abs % 20))
    Parser.tokenize(Parser.int).parse(s) == ParserSuccess(v, "")
  }
}
