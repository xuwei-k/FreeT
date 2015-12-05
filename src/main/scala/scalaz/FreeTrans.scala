package scalaz

import scalaprops.Gen
import scalaz.FreeF._
import shapeless.Lazy

/** Free Monad Transformer
 * @see [[https://github.com/ekmett/free/blob/v4.12.1/src/Control/Monad/Trans/Free.hs]]
 */
final case class FreeTrans[F[_], M[_], A](run: M[FreeF[F, A, FreeTrans[F, M, A]]]) {

  def map[B](f: A => B)(implicit F: Functor[F], M: Functor[M]): FreeTrans[F, M, B] =
    FreeTrans(M.map(run)(_.bimap(f, _.map(f))))

  def flatMap[B](f: A => FreeTrans[F, M, B])(implicit F: Functor[F], M: Monad[M]): FreeTrans[F, M, B] =
    FreeTrans(M.bind(run){
      case Pure(a) =>
        f(a).run
      case Impure(a) =>
        M.point(Impure(F.map(a)(_.flatMap(f))))
    })

  def hoist[N[_]](f: M ~> N)(implicit F: Functor[F], M: Functor[M]): FreeTrans[F, N, A] =
    FreeTrans(f(M.map(run)(_.map(_.hoist(f)))))
}

object FreeTrans extends FreeTransInstances {

  implicit def freeTEqual[F[_], M[_], A](implicit
    E: Lazy[Equal[M[FreeF[F, A, FreeTrans[F, M, A]]]]]
  ): Equal[FreeTrans[F, M, A]] = E.value.contramap(_.run)

  implicit def freeTGen[F[_], M[_], A](implicit
    E: Lazy[Gen[M[FreeF[F, A, FreeTrans[F, M, A]]]]]
  ): Gen[FreeTrans[F, M, A]] = E.value.map(FreeTrans(_))

  implicit def freeTMonadPlus[F[_], M[_]](implicit F0: Functor[F], M0: MonadPlus[M]): MonadPlus[({type l[a] = FreeTrans[F, M, a]})#l] =
    new FreeTransMonadPlus[F, M] {
      def F = F0
      def M = M0
    }

  implicit def freeTMonadHoist[F[_]: Functor]: Hoist[({type l[m[_], a] = FreeTrans[F, m, a]})#l] =
    new Hoist[({type l[m[_], a] = FreeTrans[F, m, a]})#l] {
      def hoist[M[_]: Monad, N[_]](f: M ~> N) =
        new (({type l[x] = FreeTrans[F, M, x]})#l ~> ({type l[x] = FreeTrans[F, N, x]})#l) {
          def apply[A](fa: FreeTrans[F, M, A]) =
            fa.hoist(f)
        }

      def liftM[G[_], A](a: G[A])(implicit G: Monad[G]) =
        FreeTrans(G.map(a)(FreeF.pure(_)))

      def apply[G[_]: Monad] =
        freeTMonad[F, G]
    }

  def point[F[_], M[_], A](a: => A)(implicit M: Applicative[M]): FreeTrans[F, M, A] =
    FreeTrans(M.point(Pure(a)))

  def empty[F[_], M[_], A](implicit M: PlusEmpty[M]): FreeTrans[F, M, A] =
    FreeTrans(M.empty)
}

sealed abstract class FreeTransInstances extends FreeTransInstances0 {

  implicit final def freeTMonad[F[_], M[_]](implicit F0: Functor[F], M0: Monad[M]): Monad[({type l[a] = FreeTrans[F, M, a]})#l] =
    new FreeTransMonad[F, M] {
      def F = F0
      def M = M0
    }

  implicit final def freeTPlusEmpty[F[_], M[_]](implicit M0: PlusEmpty[M]): PlusEmpty[({type l[a] = FreeTrans[F, M, a]})#l] =
    new FreeTransPlusEmpty[F, M] {
      def M = M0
    }
}

sealed abstract class FreeTransInstances0 {

  implicit final def freeTFunctor[F[_], M[_]](implicit F0: Functor[F], M0: Functor[M]): Functor[({type l[a] = FreeTrans[F, M, a]})#l] =
    new FreeTransFunctor[F, M] {
      def F = F0
      def M = M0
    }

  implicit final def freeTPlus[F[_], M[_]](implicit M0: Plus[M]): Plus[({type l[a] = FreeTrans[F, M, a]})#l] =
    new FreeTransPlus[F, M] {
      def M = M0
    }
}

private trait FreeTransFunctor[F[_], M[_]] extends Functor[({type l[a] = FreeTrans[F, M, a]})#l] {
  protected[this] implicit def F: Functor[F]
  protected[this] implicit def M: Functor[M]

  override final def map[A, B](fa: FreeTrans[F, M, A])(f: A => B) =
    fa map f

}

private trait FreeTransMonad[F[_], M[_]] extends Monad[({type l[a] = FreeTrans[F, M, a]})#l] with FreeTransFunctor[F, M] {
  protected[this] implicit def M: Monad[M]

  override final def point[A](a: => A) =
    FreeTrans.point(a)

  override final def bind[A, B](fa: FreeTrans[F, M, A])(f: A => FreeTrans[F, M, B]) =
    fa flatMap f
}

private trait FreeTransPlus[F[_], M[_]] extends Plus[({type l[a] = FreeTrans[F, M, a]})#l] {
  protected[this] implicit def M: Plus[M]

  override final def plus[A](a: FreeTrans[F, M, A], b: => FreeTrans[F, M, A]) =
    FreeTrans(M.plus(a.run, b.run))
}

private trait FreeTransPlusEmpty[F[_], M[_]] extends PlusEmpty[({type l[a] = FreeTrans[F, M, a]})#l] with FreeTransPlus[F, M]{
  protected[this] implicit def M: PlusEmpty[M]

  override final def empty[A] =
    FreeTrans.empty
}

private trait FreeTransMonadPlus[F[_], M[_]] extends MonadPlus[({type l[a] = FreeTrans[F, M, a]})#l] with FreeTransMonad[F, M] with FreeTransPlusEmpty[F, M] {
  protected[this] implicit def M: MonadPlus[M]
}

