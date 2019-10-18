package wax.exercise.parser

import java.io.File

import cats.effect.IO
import cats.implicits._

import scala.io.Source

object ConfigReader {
  val validConfigFile = "validConfig"
  val invalidConfigFile = "typicalBorisConfig"

  // TODO read 2 configs without using flatMap or for-comprehension
  def readConfigs: IO[Configs] = ???

  private def readFile(fileName: String): IO[String] = IO {
    val file = new File(this.getClass.getClassLoader.getResource(s"applicative/$fileName").toURI)
    val res = Source.fromFile(file).getLines().toSeq.head
    res
  }
}

case class Configs(validConfig: String, typicalBorisConfig: String)
