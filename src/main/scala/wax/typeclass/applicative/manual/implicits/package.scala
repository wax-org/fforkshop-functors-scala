package wax.typeclass.applicative.manual

import wax.typeclass.applicative.manual.typeclass._
import wax.typeclass.functor.manual.typeclass._

import scala.language.higherKinds

package object implicits {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(v) => Some(f(v))
        case None => None
      }

    override def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      ff match {
        case Some(f) => fa map f
        case None => None
      }
  }

  implicit val optionMonoidal: Monoidal[Option] = new Monoidal[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(v) => Some(f(v))
        case None => None
      }

    override def unit: Option[Unit] = Some(())

    override def comb[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
      (fa, fb) match {
        case (Some(a), Some(b)) => Some((a, b))
        case _                  => None
      }
      /*
      fa.fmap((a: A) => (b: B) => (a, b)) match {
        case Some(f) => fb.fmap(f)
        case None    => None
      }
     */
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case a :: as => f(a) :: fmap(as)(f)
        case Nil => Nil
      }

    override def pure[A](a: A): List[A] = List(a)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      ff match {
        case h :: tail => (fa map h) ::: ap(tail)(fa)
        case Nil => Nil
      }
  }

  implicit val listMonoidal: Monoidal[List] = new Monoidal[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case a :: as => f(a) :: fmap(as)(f)
        case Nil => Nil
      }

    override def unit: List[Unit] = List(())

    override def comb[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      fa match {
        case a :: as => fmap(fb)((b: B) => (a, b)) ::: comb(as, fb)
        case Nil     => Nil
      }
  }

  implicit def functionApplicative[A]: Applicative[Function[A, ?]] = new Applicative[Function[A, ?]] {
    override def fmap[B, C](fa: Function[A, B])(f: B => C): Function[A, C] =
      fa.andThen(f)

    override def pure[B](a: B): Function[A, B] = _ => a

    override def ap[B, C](f: Function[A, B => C])(fa: A => B): A => C =
      (a: A) => f(a)(fa(a))
  }

  implicit class ApplicativeOps[A, F[_]: Applicative](fa: F[A]) {
    def ap[B](f: F[A => B]): F[B] = Applicative[F].ap(f)(fa)
  }
}
