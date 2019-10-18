package wax.typeclass.functor.manual

import scala.language.higherKinds

package object typeclass {

  //
  // Laws:
  // 1. Identity:
  //    (fa: F[A]) =>
  //      fa.map(identity) <-> fa
  // 2. Composition:
  //    (fa: F[A], f: A => B, g: B => C) =>
  //      fa.map(f).map(g) <-> fa.map(f andThen g)
  //
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

}
