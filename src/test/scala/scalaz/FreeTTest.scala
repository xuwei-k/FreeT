package scalaz

import scalaprops._
import scalaz.Id.Id
import scalaz.std.anyVal._

object FreeTransTest extends Scalaprops {

  private[this] type OneOrTwo[A] = OneAnd[Maybe, A]

  val monadTransMaybe =
    scalazlaws.monadTrans.all[({type l[m[_], a] = FreeTrans[Maybe, m, a]})#l]

  val monadTransIList =
    scalazlaws.monadTrans.all[({type l[m[_], a] = FreeTrans[IList, m, a]})#l]

  val idId = {
    type F[A] = FreeTrans[Id, Id, A]
    scalazlaws.monad.all[F]
  }

  val idIList = {
    type F[A] = FreeTrans[Id, IList, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(5))

  val idNel = {
    type F[A] = FreeTrans[Id, NonEmptyList, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }.andThenParam(Param.maxSize(3))

  val idDisjunction = {
    type G[A] = Int \/ A
    type F[A] = FreeTrans[Id, G, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val disjunctionDisjunction = {
    type G[A] = Int \/ A
    type F[A] = FreeTrans[G, G, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val maybeMaybe = {
    type F[A] = FreeTrans[Maybe, Maybe, A]
    scalazlaws.monadPlus.all[F]
  }

  val maybeIList = {
    type F[A] = FreeTrans[Maybe, IList, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(5))

  val iListMaybe = {
    type F[A] = FreeTrans[IList, Maybe, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(5))

  val iListIList = {
    type F[A] = FreeTrans[IList, IList, A]
    scalazlaws.monadPlus.all[F]
  }.andThenParam(Param.maxSize(2))

  val iListId = {
    type F[A] = FreeTrans[IList, Id, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(5))

  val maybeOneOrTwo = {
    type F[A] = FreeTrans[Maybe, OneOrTwo, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val oneOrTwoMaybe = {
    type F[A] = FreeTrans[OneOrTwo, Maybe, A]
    scalazlaws.monadPlus.all[F]
  }
}
