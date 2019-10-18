package wax.exercise.parser

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import wax.exercise.parser.ConfigParser._

object ConfigParserSpec extends Properties("Parser") {
  property("config parser should parse config") = forAll { cfg: Config =>
    val stream = Seq[String](
      cfg.appPort.toString,
      cfg.dbHost,
      cfg.dbUsername,
      cfg.dbPw,
      cfg.dbSchema,
      cfg.dbPort.toString,
    ).mkString(",")
    config.parse(stream) == ParserSuccess(cfg, "")
  }

  property("spaces must not affect the parsing") = forAll { (cfg: Config, spacesCount: Int) =>
    val spaces = 0.until(((spacesCount % 10) + 10) % 10).map(_ => ' ').mkString
    val stream = Seq[String](
      cfg.appPort.toString,
      cfg.dbHost,
      cfg.dbUsername,
      cfg.dbPw,
      cfg.dbSchema,
      cfg.dbPort.toString,
    ).mkString(spaces ++ "," ++ spaces)
    config.parse(stream) == ParserSuccess(cfg, "")
  }

  implicit val arbConfig: Arbitrary[Config] = Arbitrary(genConfig)

  def genConfig: Gen[Config] = for {
    appPort <- Gen.posNum[Int]
    dbHost1 <- Gen.posNum[Int]
    dbHost2 <- Gen.posNum[Int]
    dbHost3 <- Gen.posNum[Int]
    dbHost4 <- Gen.posNum[Int]
    dbUsername <- Gen.alphaStr
    dbPw <- Gen.alphaNumStr
    dbSchema <- Gen.alphaStr
    dbPort <- Gen.posNum[Int]
  } yield Config(
    appPort = appPort,
    dbHost = s"$dbHost1.$dbHost2.$dbHost3.$dbHost4",
    dbUsername = dbUsername,
    dbPw = dbPw,
    dbSchema = dbSchema,
    dbPort = dbPort
  )

}
