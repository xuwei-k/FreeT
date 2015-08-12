package scalaz

import scalaprops.Gen

/**
 * @see [[https://github.com/ekmett/free/blob/v4.12.1/src/Control/Monad/Trans/Free/Church.hs]]
 */
abstract class FT[F[_], M[_], A] {
  self =>

  def run[R](x: A => M[R], y: Forall[({type l[X] = (X => M[R]) => F[X] => M[R]})#l]): M[R]

  final def map[B](f: A => B): FT[F, M, B] =
    new FT[F, M, B] {
      def run[R](x: B => M[R], y: Forall[({type l[X] = (X => M[R]) => F[X] => M[R]})#l]) =
        self.run(f andThen x, y)
    }

  final def flatMap[B](f: A => FT[F, M, B]): FT[F, M, B] =
    new FT[F, M, B] {
      def run[R](b: B => M[R], fr: Forall[({type l[X] = (X => M[R]) => F[X] => M[R]})#l]) =
        self.run(f(_).run(b, fr), fr)
    }

  final def toFreeT(implicit F: Functor[F], M: Applicative[M]): FreeT[F, M, A] =
    FreeT(run(
      a => M.point(FreeF.pure(a)),
      new Forall[({type l[X] = (X => M[FreeF[F, A, FreeT[F, M, A]]]) => F[X] => M[FreeF[F, A, FreeT[F, M, A]]]})#l] {
        def apply[B] = { xg => fx =>
          M.point(FreeF.impure(F.map(fx)(xg.andThen(FreeT(_)))))
        }
      }
    ))
}

object FT {

  def fromFreeT[F[_], M[_], A](f: FreeT[F, M, A])(implicit M: Monad[M]): FT[F, M, A] =
    new FT[F, M, A] {
      def run[R](ka: A => M[R], kfr: Forall[({type l[X] = (X => M[R]) => F[X] => M[R]})#l]) =
        M.bind(f.run){
          case FreeF.Pure(a) =>
            ka(a)
          case FreeF.Impure(fb) =>
            kfr[FreeT[F, M, A]](fromFreeT(_).run(ka, kfr)).apply(fb)
        }
      }

  implicit def equalFT[F[_], M[_], A](implicit
    F: Functor[F],
    M: Monad[M],
    E: Equal[FreeT[F, M, A]]
  ): Equal[FT[F, M, A]] = E.contramap(_.toFreeT)

  implicit def genFT[F[_], M[_], A](implicit
    F: Functor[F],
    M: Monad[M],
    G: Gen[FreeT[F, M, A]]
  ): Gen[FT[F, M, A]] = G.map(fromFreeT(_))

  implicit def instance[F[_], M[_]]: Monad[({type l[a] = FT[F, M, a]})#l] =
    new Monad[({type l[a] = FT[F, M, a]})#l] {
      override def point[A](a: => A) =
        new FT[F, M, A] {
          def run[R](x: A => M[R], y: Forall[({type l[X] = ((X) => M[R]) => (F[X]) => M[R]})#l]) =
            x(a)
        }

      override def map[A, B](fa: FT[F, M, A])(f: A => B) =
        fa map f

      override def bind[A, B](fa: FT[F, M, A])(f: A => FT[F, M, B]) =
        fa flatMap f
    }

}
