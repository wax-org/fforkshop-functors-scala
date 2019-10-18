package wax.exercise.parser

import cats.data.NonEmptyList
import wax.exercise.parser.ConfigValidator._

object Application extends App {

  val Configs(validConfigStr, invalidConfigStr) = ConfigReader.readConfigs.unsafeRunSync()

  val validConfig = ConfigParser.parseConfig(validConfigStr)
  val invalidConfig = ConfigParser.parseConfig(invalidConfigStr)

  assert(ConfigValidator.validateConfig(validConfig) ==
    Valid(Config(8080, "127.0.0.1", "root", "sa", "rkt", 3300)))

  assert(ConfigValidator.validateConfig(invalidConfig) ==
    Invalid(NonEmptyList.of(
      ConfigValidationError("appPort", "port must be an int between 0 and 65536"),
      ConfigValidationError("dbHost", "host must be a proper hostname/ip without port")
    )))
}
