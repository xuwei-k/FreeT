package scalaz

import shapeless.Lazy

import scalaprops.{Cogen, Gen}

final case class CofreeF[F[_], A, B](head: A, tail: F[B]) {
  def bimap[AA, BB](f: A => AA, g: B => BB)(implicit F: Functor[F]): CofreeF[F, AA, BB] =
    CofreeF(f(head), F.map(tail)(g))
}

object CofreeF {
  implicit def cofreeFEqual[F[_], A, B](implicit A: Lazy[Equal[A]], F: Lazy[Equal[F[B]]]): Equal[CofreeF[F, A, B]] =
    Equal.equal { (x, y) =>
      A.value.equal(x.head, y.head) && F.value.equal(x.tail, y.tail)
    }

  implicit def cofreeFGen[F[_], A, B](implicit A: Gen[A], F: Lazy[Gen[F[B]]]): Gen[CofreeF[F, A, B]] =
    Apply[Gen].apply2(A, F.value)(CofreeF(_, _))

  implicit def cofreeFCogen[F[_], A, B](implicit A: Lazy[Cogen[A]], F: Lazy[Cogen[F[B]]]): Cogen[CofreeF[F, A, B]] = {
    implicit def f = F.value
    implicit def a = A.value
    Cogen.from2(unapply)
  }
}
