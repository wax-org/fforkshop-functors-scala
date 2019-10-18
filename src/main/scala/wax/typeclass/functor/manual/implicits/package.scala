package wax.typeclass.functor.manual

import wax.typeclass.functor.manual.typeclass.Functor

import scala.language.higherKinds

package object implicits {
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = ???
  }

  implicit def eitherFunctor[E]: Functor[Either[E, ?]] = new Functor[Either[E, ?]] {
    override def fmap[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = ???
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = ???
  }

  implicit def functionFunctor[A]: Functor[Function[A, ?]] = new Functor[Function[A, ?]] {
    override def fmap[B, C](fa: A => B)(f: B => C): A => C = ???
  }
}
