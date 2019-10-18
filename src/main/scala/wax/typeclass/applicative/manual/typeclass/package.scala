package wax.typeclass.applicative.manual

import wax.typeclass.functor.manual.typeclass._

import scala.language.higherKinds

package object typeclass {

  //
  // Laws:
  // 1. Identity:
  //    (fa: F[A]) =>
  //      F.pure((a: A) => a).ap(fa) <-> fa
  // 2. Homomorphism:
  //    (a: A, f: A => B) =>
  //      F.pure(f).ap(F.pure(a)) <-> F.pure(f(a))
  // 3. Interchange:
  //    (a: A, ff: F[A => B]) =>
  //      ff.ap(F.pure(a)) <-> F.pure((f: A => B) => f(a)).ap(ff)
  // 4. Map:
  //    (fa: F[A], f: A => B) =>
  //      fa.map(f) <-> F.pure(f).ap(fa)
  // 5. Composition:
  //    (fa: F[A], fab: F[A => B], fbc: F[B => C]) => {
  //      val compose: (B => C) => (A => B) => (A => C) = _.compose
  //      F.pure(compose).ap(fbc).ap(fab).ap(fa) <-> fbc.ap(fab.ap(fa))
  //    }
  //
  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def ap[A, B](f: F[A => B])(fa: F[A]): F[B]
  }

  object Applicative {
    def apply[F[_] : Applicative]: Applicative[F] = implicitly[Applicative[F]]
  }

  implicit class ApplicativeOps[F[_]: Applicative, A, B](ff: F[A => B]) {
    def ap(fa: F[A]): F[B] = Applicative[F].ap(ff)(fa)
  }

  trait Monoidal[F[_]] extends Functor[F] {
    def unit: F[Unit]
    def comb[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Monoidal {
    def apply[F[_]: Monoidal]: Monoidal[F] = implicitly[Monoidal[F]]
  }

  class MonoidalToApplicative[F[_]: Monoidal]() extends Applicative[F] {
    override def fmap[A, B](fa: F[A])(f: A => B): F[B] = Monoidal[F].fmap(fa)(f)

    override def pure[A](a: A): F[A] = Monoidal[F].unit.fmap(_ => a)

    override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = Monoidal[F].comb(ff, fa).fmap { case (f, a) => f(a) }
  }

  class ApplicativeToMonoida[F[_]: Applicative]() extends Monoidal[F] {
    override def fmap[A, B](fa: F[A])(f: A => B): F[B] = Applicative[F].fmap(fa)(f)

    override def unit: F[Unit] = Applicative[F].pure(())

    override def comb[A, B](fa: F[A], fb: F[B]): F[(A, B)] = fa.fmap((a: A) => (b: B) => (a, b)).ap(fb)
  }

}
