package scalaz

import scalaprops.Gen
import scalaz.FMP._
import scalaz.FreeF._

sealed abstract class FMP[F[_], A] extends Product with Serializable {

  final def asFreeT(implicit F: Functor[F]): FreeT[F, IList, A] = {
    val G = FreeT.freeTMonadPlus[F, IList]
    this match {
      case FNil() =>
        FreeT.empty
      case ConsV(a, f) =>
        G.plus(FreeT.point(a), f.asFreeT)
      case ConsF(a, f) =>
        G.plus(
          FreeT(IList.single(FreeF.impure(F.map(a)(_.asFreeT)))),
          f.asFreeT
        )
    }
  }
}

object FMP {
  private[scalaz] final case class FNil[F[_], A]() extends FMP[F, A]
  private[scalaz] final case class ConsV[F[_], A](a: A, f: FMP[F, A]) extends FMP[F, A]
  private[scalaz] final case class ConsF[F[_], A](a: F[FMP[F, A]], f: FMP[F, A]) extends FMP[F, A]

  implicit def fmpEqual[F[_], A](implicit
    E1: Equal[A],
    E2: shapeless.Lazy[Equal[FMP[F, A]]],
    E3: shapeless.Lazy[Equal[F[FMP[F, A]]]]
  ): Equal[FMP[F, A]] = Equal.equal{
    case (FNil(), FNil()) =>
      true
    case (ConsV(a1, f1), ConsV(a2, f2)) =>
      E1.equal(a1, a2) && E2.value.equal(f1, f2)
    case (ConsF(a1, f1), ConsF(a2, f2)) =>
      E3.value.equal(a1, a2) && E2.value.equal(f1, f2)
    case _ =>
      false
  }

  implicit def FMPGen[F[_], A](implicit
    G1: Gen[A],
    G2: shapeless.Lazy[Gen[FMP[F, A]]],
    G3: shapeless.Lazy[Gen[F[FMP[F, A]]]]
  ): Gen[FMP[F, A]] =
    Gen.oneOf(
      Gen.value(FNil()),
      Apply[Gen].apply2(G1, G2.value)(ConsV(_, _)),
      Apply[Gen].apply2(G3.value, G2.value)(ConsF(_, _))
    )

  import scalaz.Isomorphism._

  def freeTFMPIso[F[_]](implicit F: Functor[F]): ({type l[a] = FMP[F, a]})#l <~> ({type l[a] = FreeT[F, IList, a]})#l =
    new IsoFunctorTemplate[({type l[a] = FMP[F, a]})#l, ({type l[a] = FreeT[F, IList, a]})#l] {
      def to[A](fa: FMP[F, A]) =
        fa.asFreeT

      def from[A](ga: FreeT[F, IList, A]) = {
        def loop(list: IList[FreeF[F, A, FreeT[F, IList, A]]]): FMP[F, A] =
          list match {
            case ICons(h, t) => h match {
              case Pure(a) =>
                ConsV(a, loop(t))
              case Impure(a) =>
                ConsF(F.map(a)(from(_)), loop(t))
            }
            case INil() =>
              FNil()
          }
        loop(ga.run)
      }
    }
}