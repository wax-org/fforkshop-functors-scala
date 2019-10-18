package wax.utils

import java.time
import java.time.Duration
import java.time.LocalDateTime.now

import cats.Functor

object Benchmark {
  case class BenchmarkResult[A](result: A, duration: Duration) {
    override def toString: String =
      s"""duration = ${duration.toMillis} ms
         |result   = $result""".stripMargin
  }

  implicit val benchmarkResultFunctor: Functor[BenchmarkResult] = new Functor[BenchmarkResult] {
    override def map[A, B](fa: BenchmarkResult[A])(f: A => B): BenchmarkResult[B] =
      BenchmarkResult(f(fa.result), fa.duration)
  }

  def apply[A](f: => A): BenchmarkResult[A] = {
    val s = now()
    val r = f
    val e = now()
    BenchmarkResult(r, time.Duration.between(s, e))
  }
}
