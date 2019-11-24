package wax.typeclass.applicative

import cats.Applicative
import cats.kernel.Monoid
import cats.syntax.functor._
import wax.typeclass.functor.implicits._

package object implicits {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = optionFunctor.map(fa)(f)

    override def pure[A](x: A): Option[A] = ???

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ???
  }

  implicit def eitherApplicative[E]: Applicative[Either[E, ?]] = new Applicative[Either[E, ?]] {
    override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = eitherFunctor.map(fa)(f)

    override def pure[A](x: A): Either[E, A] = ???

    override def ap[A, B](ff: Either[E, A => B])(fa: Either[E, A]): Either[E, B] = ???
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = listFunctor.map(fa)(f)

    override def pure[A](x: A): List[A] = ???

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ???
  }

  implicit def tupleApplicative[C: Monoid]: Applicative[(C, ?)] = new Applicative[Tuple2[C, ?]] {
    override def map[A, B](fa: (C, A))(f: A => B): (C, B) = tupleFunctor.map(fa)(f)

    override def pure[A](x: A): (C, A) = ???

    override def ap[A, B](ff: (C, A => B))(fa: (C, A)): (C, B) = ???
  }

  implicit def functionApplicative[T]: Applicative[Function[T, ?]] = new Applicative[Function[T, ?]] {
    override def map[A, B](fa: Function[T, A])(f: A => B): Function[T, B] = functionFunctor.map(fa)(f)

    override def pure[A](x: A): Function[T, A] = ???

    override def ap[A, B](ff: Function[T, A => B])(fa: Function[T, A]): Function[T, B] = ???
  }
}
