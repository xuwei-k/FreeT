package scalaz

import scalaprops._
import scalaz.Id.Id
import scalaz.std.anyVal._

object FreeTTest extends Scalaprops {

  private[this] type OneOrTwo[A] = OneAnd[Maybe, A]

  val isoFMPMaybe =
    scalazlaws.iso.all(FMP.freeTFMPIso[Maybe].unlift[Int]).andThenParam(Param.maxSize(5))

  val monadTransMaybe =
    scalazlaws.monadTrans.all[({type l[m[_], a] = FreeT[Maybe, m, a]})#l]

  val monadTransIList =
    scalazlaws.monadTrans.all[({type l[m[_], a] = FreeT[IList, m, a]})#l]

  val idId = {
    type F[A] = FreeT[Id, Id, A]
    scalazlaws.monad.all[F]
  }

  val idIList = {
    type F[A] = FreeT[Id, IList, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(5))

  val idNel = {
    type F[A] = FreeT[Id, NonEmptyList, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }.andThenParam(Param.maxSize(3))

  val idDisjunction = {
    type G[A] = Int \/ A
    type F[A] = FreeT[Id, G, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val disjunctionDisjunction = {
    type G[A] = Int \/ A
    type F[A] = FreeT[G, G, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val maybeMaybe = {
    type F[A] = FreeT[Maybe, Maybe, A]
    scalazlaws.monadPlus.all[F]
  }

  val maybeIList = {
    type F[A] = FreeT[Maybe, IList, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(5))

  val iListMaybe = {
    type F[A] = FreeT[IList, Maybe, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(5))

  val iListIList = {
    type F[A] = FreeT[IList, IList, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(2))

  val iListId = {
    type F[A] = FreeT[IList, Id, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(5))

  val maybeOneOrTwo = {
    type F[A] = FreeT[Maybe, OneOrTwo, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val oneOrTwoMaybe = {
    type F[A] = FreeT[OneOrTwo, Maybe, A]
    scalazlaws.monadPlus.all[F]
  }
}
