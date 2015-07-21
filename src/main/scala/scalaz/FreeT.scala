package scalaz

import scalaprops.Gen
import scalaz.FreeF._
import shapeless.Lazy

/** Free Monad Transformer
 * @see [[https://github.com/ekmett/free/blob/v4.12.1/src/Control/Monad/Trans/Free.hs]]
 */
final case class FreeT[F[_], M[_], A](run: M[FreeF[F, A, FreeT[F, M, A]]]) {

  def map[B](f: A => B)(implicit F: Functor[F], M: Functor[M]): FreeT[F, M, B] =
    FreeT(M.map(run)(_.bimap(f, _.map(f))))

  def flatMap[B](f: A => FreeT[F, M, B])(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, B] =
    FreeT(M.bind(run){
      case Pure(a) =>
        f(a).run
      case Impure(a) =>
        M.point(Impure(F.map(a)(_.flatMap(f))))
    })

  def hoist[N[_]](f: M ~> N)(implicit F: Functor[F], M: Functor[M]): FreeT[F, N, A] =
    FreeT(f(M.map(run)(_.map(_.hoist(f)))))
}

object FreeT extends FreeTInstances {

  implicit def freeTEqual[F[_], M[_], A](implicit
    E: Lazy[Equal[M[FreeF[F, A, FreeT[F, M, A]]]]]
  ): Equal[FreeT[F, M, A]] = E.value.contramap(_.run)

  implicit def freeTGen[F[_], M[_], A](implicit
    E: Lazy[Gen[M[FreeF[F, A, FreeT[F, M, A]]]]]
  ): Gen[FreeT[F, M, A]] = E.value.map(FreeT(_))

  implicit def freeTMonad[F[_], M[_]](implicit F0: Functor[F], M0: Monad[M]): Monad[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTMonad[F, M] {
      def F = F0
      def M = M0
    }

  implicit def freeTMonadHoist[F[_]: Functor]: Hoist[({type l[m[_], a] = FreeT[F, m, a]})#l] =
    new Hoist[({type l[m[_], a] = FreeT[F, m, a]})#l] {
      def hoist[M[_]: Monad, N[_]](f: M ~> N) =
        new (({type l[x] = FreeT[F, M, x]})#l ~> ({type l[x] = FreeT[F, N, x]})#l) {
          def apply[A](fa: FreeT[F, M, A]) =
            fa.hoist(f)
        }

      def liftM[G[_], A](a: G[A])(implicit G: Monad[G]) =
        FreeT(G.map(a)(FreeF.pure(_)))

      def apply[G[_]: Monad] =
        freeTMonad[F, G]
    }

  def point[F[_], M[_], A](a: => A)(implicit M: Applicative[M]): FreeT[F, M, A] =
    FreeT(M.point(Pure(a)))
}

sealed abstract class FreeTInstances {

  implicit final def freeTFunctor[F[_], M[_]](implicit F0: Functor[F], M0: Functor[M]): Functor[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTFunctor[F, M] {
      def F = F0
      def M = M0
    }

}

private trait FreeTFunctor[F[_], M[_]] extends Functor[({type l[a] = FreeT[F, M, a]})#l] {
  protected[this] implicit def F: Functor[F]
  protected[this] implicit def M: Functor[M]

  override final def map[A, B](fa: FreeT[F, M, A])(f: A => B) =
    fa map f

}

private trait FreeTMonad[F[_], M[_]] extends Monad[({type l[a] = FreeT[F, M, a]})#l] with FreeTFunctor[F, M] {
  protected[this] implicit def M: Monad[M]

  override final def point[A](a: => A) =
    FreeT.point(a)

  override final def bind[A, B](fa: FreeT[F, M, A])(f: A => FreeT[F, M, B]) =
    fa flatMap f
}

