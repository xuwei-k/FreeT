package scalaz

import shapeless.Lazy

import scalaprops.{Cogen, CogenState, Gen}

final case class CofreeT[F[_], W[_], A](run: W[CofreeF[F, A, CofreeT[F, W, A]]]) {
  def map[B](f: A => B)(implicit F: Functor[F], W: Functor[W]): CofreeT[F, W, B] =
    CofreeT(W.map(run)(_.bimap(f, _.map(f))))

  def cobind[B](f: CofreeT[F, W, A] => B)(implicit F: Functor[F], W: Comonad[W]): CofreeT[F, W, B] =
    CofreeT(W.extend(run)(w =>
      CofreeF(f(CofreeT(w)), F.map(W.copoint(run).tail)(_.cobind(f)))
    ))

  def copoint(implicit W: Comonad[W]): A =
    W.copoint(run).head
}

object CofreeT extends CofreeTInstances {
  implicit def cofreeTComonad[F[_]: Functor, W[_]: Comonad]: Comonad[({type l[a] = CofreeT[F, W, a]})#l] =
    new Comonad[({type l[a] = CofreeT[F, W, a]})#l] {
      override def copoint[A](p: CofreeT[F, W, A]) =
        p.copoint

      override def cobind[A, B](fa: CofreeT[F, W, A])(f: CofreeT[F, W, A] => B) =
        fa cobind f

      override def map[A, B](fa: CofreeT[F, W, A])(f: A => B) =
        fa map f
    }

  implicit def cofreeTEqual[F[_], W[_], A](
    implicit W: Lazy[Equal[W[CofreeF[F, A, CofreeT[F, W, A]]]]]
  ): Equal[CofreeT[F, W, A]] =
    new Equal[CofreeT[F, W, A]] {
      def equal(a: CofreeT[F, W, A], b: CofreeT[F, W, A]) =
        W.value.equal(a.run, b.run)
    }

  implicit def cofreeTGen[F[_], W[_], A](
    implicit W: Lazy[Gen[W[CofreeF[F, A, CofreeT[F, W, A]]]]]
  ): Gen[CofreeT[F, W, A]] =
    W.value.map(CofreeT(_))

  implicit def cofreeTCogen[F[_], W[_], A](
    implicit W: Lazy[Cogen[W[CofreeF[F, A, CofreeT[F, W, A]]]]]
  ): Cogen[CofreeT[F, W, A]] =
    new Cogen[CofreeT[F, W, A]] {
      def cogen[B](a: CofreeT[F, W, A], g: CogenState[B]) =
        W.value.cogen(a.run, g)
    }
}


sealed abstract class CofreeTInstances {

  implicit def cofreeTFunctor[F[_]: Functor, W[_]: Functor]: Functor[({type l[a] = CofreeT[F, W, a]})#l] =
    new Functor[({type l[a] = CofreeT[F, W, a]})#l] {
      override def map[A, B](fa: CofreeT[F, W, A])(f: A => B) =
        fa map f
    }

}
