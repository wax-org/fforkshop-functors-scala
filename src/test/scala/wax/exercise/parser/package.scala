package wax.exercise

import org.scalacheck.{Arbitrary, Gen}

package object parser {
  case class AdequateString(value: String) {
    override def toString: String = value.toString
  }

  implicit val arbAdequateString: Arbitrary[AdequateString] = Arbitrary(Gen.alphaNumStr.map(AdequateString.apply))

  case class DigitChar(value: Char) {
    override def toString: String = value.toString
  }

  implicit val arbDigitChar: Arbitrary[DigitChar] = Arbitrary(Gen.numChar.map(DigitChar.apply))

  case class NonDigitChar(value: Char) {
    override def toString: String = value.toString
  }

  implicit val arbNonDigitChar: Arbitrary[NonDigitChar] = Arbitrary(Gen.alphaChar.map(NonDigitChar.apply))

  case class Nat(value: Int) {
    override def toString: String = value.toString
  }

  implicit val arbNat: Arbitrary[Nat] = Arbitrary(Gen.posNum[Int].map(Nat.apply))
}
