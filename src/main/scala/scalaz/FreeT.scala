package scalaz

import scalaprops.Gen
import scalaz.FreeF._
import shapeless.Lazy

import scalaz.FreeT.{FlatMap, Wrap}

/** Free Monad Transformer
 * @see [[https://github.com/ekmett/free/blob/v4.12.1/src/Control/Monad/Trans/Free.hs]]
 */
sealed abstract class FreeT[F[_], M[_], A] {

  def map[B](f: A => B)(implicit M: Applicative[M]): FreeT[F, M, B] = flatMap(a => Wrap(M.pure(Pure(f(a)))))

  def flatMap[B](f: A => FreeT[F, M, B]): FreeT[F, M, B] =
    this match {
      case fa@Wrap(_) => FreeT.flatMap(fa)(f)
      case b @ FlatMap() => FreeT.flatMap(b.fa)(a => FreeT.flatMap(b.f(a))(f))
    }

  @annotation.tailrec
  final def run(implicit F: Applicative[F], M: Functor[M]): M[FreeF[F, A, FreeT[F, M, A]]] =
    this match {
      case Wrap(m) => m
      case x @ FlatMap() =>
        x.fa match {
          case Wrap(m) =>
            M.map(m) {
              case Pure(a) => Impure(F.pure(x.f(a)))
              case Impure(fb) => Impure(F.map(fb)(_.flatMap(x.f)))
            }
          case a @ FlatMap() =>
            a.fa.flatMap(b => a.f(b).flatMap(x.f)).run
        }
    }

  def hoist[N[_]](f: M ~> N)(implicit F: Applicative[F], M: Functor[M]): FreeT[F, N, A] =
    Wrap(f(M.map(run)(_.map(_.hoist(f)))))
}


object FreeT extends FreeTInstances {

  private[scalaz] final case class Wrap[F[_], M[_], A](run: M[FreeF[F, A, FreeT[F, M, A]]]) extends FreeT[F, M, A]

  private[scalaz] sealed abstract case class FlatMap[F[_], M[_], B]() extends FreeT[F, M, B]{
    type A
    val fa: FreeT[F, M, A]
    val f: A => FreeT[F, M, B]
  }

  def apply[F[_], M[_], A](value: M[FreeF[F, A, FreeT[F, M, A]]]): FreeT[F, M, A] =
    Wrap(value)

  def flatMap[F[_], M[_], A0, B](fa0: FreeT[F, M, A0])(f0: A0 => FreeT[F, M, B]): FreeT[F, M, B] =
    new FlatMap[F, M, B] {
      type A = A0
      val fa = fa0
      val f = f0
    }

  implicit def freeTEqual[F[_]: Applicative, M[_]: Functor, A](implicit
    E: Lazy[Equal[M[FreeF[F, A, FreeT[F, M, A]]]]]
  ): Equal[FreeT[F, M, A]] = E.value.contramap(_.run)

  implicit def freeTGen[F[_], M[_], A](implicit
    E: Lazy[Gen[M[FreeF[F, A, FreeT[F, M, A]]]]]
  ): Gen[FreeT[F, M, A]] = E.value.map(FreeT(_))

  implicit def freeTMonadPlus[F[_], M[_]](implicit F0: Applicative[F], M0: MonadPlus[M]): MonadPlus[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTMonadPlus[F, M] {
      def F = F0
      def M = M0
    }

  implicit def freeTMonadHoist[F[_]: Applicative]: Hoist[({type l[m[_], a] = FreeT[F, m, a]})#l] =
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

  def empty[F[_], M[_], A](implicit M: PlusEmpty[M]): FreeT[F, M, A] =
    FreeT(M.empty)
}

sealed abstract class FreeTInstances extends FreeTInstances0 {

  implicit final def freeTMonad[F[_], M[_]](implicit F0: Functor[F], M0: Monad[M]): Monad[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTMonad[F, M] {
      def F = F0
      def M = M0
    }

  implicit final def freeTPlusEmpty[F[_], M[_]](implicit M0: PlusEmpty[M], M1: Functor[M], F0: Applicative[F]): PlusEmpty[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTPlusEmpty[F, M] {
      override def M = M0
      override def F = F0
      override def N = M1
    }
}

sealed abstract class FreeTInstances0 {

  implicit final def freeTFunctor[F[_], M[_]](implicit F0: Functor[F], M0: Applicative[M]): Functor[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTFunctor[F, M] {
      def F = F0
      def M = M0
    }

  implicit final def freeTPlus[F[_], M[_]](implicit M0: Plus[M], M1: Functor[M], F0: Applicative[F]): Plus[({type l[a] = FreeT[F, M, a]})#l] =
    new FreeTPlus[F, M] {
      override def M = M0
      override def F = F0
      override def N = M1
    }
}

private trait FreeTFunctor[F[_], M[_]] extends Functor[({type l[a] = FreeT[F, M, a]})#l] {
  protected[this] implicit def F: Functor[F]
  protected[this] implicit def M: Applicative[M]

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

private trait FreeTPlus[F[_], M[_]] extends Plus[({type l[a] = FreeT[F, M, a]})#l] {
  protected[this] implicit def F: Applicative[F]
  protected[this] implicit def M: Plus[M]
  protected[this] implicit def N: Functor[M]

  override final def plus[A](a: FreeT[F, M, A], b: => FreeT[F, M, A]) =
    FreeT(M.plus(a.run, b.run))
}

private trait FreeTPlusEmpty[F[_], M[_]] extends PlusEmpty[({type l[a] = FreeT[F, M, a]})#l] with FreeTPlus[F, M]{
  protected[this] implicit override def M: PlusEmpty[M]

  override final def empty[A] =
    FreeT.empty
}

private trait FreeTMonadPlus[F[_], M[_]] extends MonadPlus[({type l[a] = FreeT[F, M, a]})#l] with FreeTMonad[F, M] with FreeTPlusEmpty[F, M] {
  protected[this] implicit def M: MonadPlus[M]
  protected[this] override def N = M
}

