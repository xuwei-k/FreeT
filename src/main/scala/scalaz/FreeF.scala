package scalaz

import scalaprops.Gen
import scalaz.FreeF._
import shapeless.Lazy

sealed abstract class FreeF[F[_], A, B] extends Product with Serializable {

  final def map[C](f: B => C)(implicit F: Functor[F]): FreeF[F, A, C] =
    this match {
      case a @ Pure(_) =>
        a.cast[F, C]
      case Impure(a) =>
        Impure(F.map(a)(f))
    }

  final def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): FreeF[F, C, D] =
    this match {
      case Pure(a) =>
        Pure(f(a))
      case Impure(a) =>
        Impure(F.map(a)(g))
    }
}

object FreeF {

  private[scalaz] final case class Pure[F[_], A, B](a: A) extends FreeF[F, A, B] {
    def cast[G[_], C]: FreeF[G, A, C] =
      this.asInstanceOf[FreeF[G, A, C]]
  }
  private[scalaz] final case class Impure[F[_], A, B](a: F[B]) extends FreeF[F, A, B]

  def pure[F[_], A, B](a: A): FreeF[F, A, B] = Pure(a)
  def impure[F[_], A, B](fb: F[B]): FreeF[F, A, B] = Impure(fb)

  implicit def freeFGen[F[_], A, B](implicit
    A: Lazy[Gen[A]],
    B: Lazy[Gen[F[B]]]
  ): Gen[FreeF[F, A, B]] =
    Gen.oneOfLazy(
      Need(A.value.map(Pure(_))),
      Need(B.value.map(Impure(_)))
    )

  implicit def freeFEqual[F[_], A, B](implicit
    A: Lazy[Equal[A]],
    B: Lazy[Equal[F[B]]]
  ): Equal[FreeF[F, A, B]] =
    Equal.equal {
      case (Pure(a1), Pure(a2)) =>
        A.value.equal(a1, a2)
      case (Impure(a1), Impure(a2)) =>
        B.value.equal(a1, a2)
      case _ =>
        false
    }

  implicit def freeFFunctor[F[_]: Functor, C]: Functor[({type l[a] = FreeF[F, C, a]})#l] =
    new Functor[({type l[a] = FreeF[F, C, a]})#l] {
      def map[A, B](fa: FreeF[F, C, A])(f: A => B) =
        fa map f
    }

  implicit def freeFBifunctor[F[_]: Functor]: Bifunctor[({type l[a, b] = FreeF[F, a, b]})#l] =
    new Bifunctor[({type l[a, b] = FreeF[F, a, b]})#l] {
      def bimap[A, B, C, D](fab: FreeF[F, A, B])(f: A => C, g: B => D) =
        fab.bimap(f, g)
    }
}
