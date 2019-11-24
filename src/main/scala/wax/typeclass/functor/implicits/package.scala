package wax.typeclass.functor

import cats.Functor

package object implicits {
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = ???
  }

  implicit def eitherFunctor[E]: Functor[Either[E, ?]] = new Functor[Either[E, ?]] {
    override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = ???
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = ???
  }

  implicit def tupleFunctor[C]: Functor[(C, ?)] = new Functor[Tuple2[C, ?]] {
    override def map[A, B](fa: (C, A))(f: A => B): (C, B) = ???
  }

  implicit def functionFunctor[A]: Functor[Function[A, ?]] = new Functor[Function[A, ?]] {
    override def map[B, C](fa: A => B)(f: B => C): A => C = ???
  }
}
