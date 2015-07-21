package scalaz

import scalaprops._
import scalaz.Id.Id
import scalaz.std.anyVal._

object FreeTTest extends Scalaprops {

  private[this] type OneOrTwo[A] = OneAnd[Maybe, A]

  val idId = {
    type F[A] = FreeT[Id, Id, A]
    scalazlaws.monad.all[F]
  }

  val idIList = {
    type F[A] = FreeT[Id, IList, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(5))

  val idNel = {
    type F[A] = FreeT[Id, NonEmptyList, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(3))

  val idDisjunction = {
    type G[A] = Int \/ A
    type F[A] = FreeT[Id, G, A]
    scalazlaws.monad.all[F]
  }

  val disjunctionDisjunction = {
    type G[A] = Int \/ A
    type F[A] = FreeT[G, G, A]
    scalazlaws.monad.all[F]
  }

  val maybeMaybe = {
    type F[A] = FreeT[Maybe, Maybe, A]
    scalazlaws.monad.all[F]
  }

  val maybeIList = {
    type F[A] = FreeT[Maybe, IList, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(5))

  val iListMaybe = {
    type F[A] = FreeT[IList, Maybe, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(5))

  val iListIList = {
    type F[A] = FreeT[IList, IList, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(2))

  val iListId = {
    type F[A] = FreeT[IList, Id, A]
    scalazlaws.monad.all[F]
  }.andThenParam(Param.maxSize(5))

  val maybeOneOrTwo = {
    type F[A] = FreeT[Maybe, OneOrTwo, A]
    scalazlaws.monad.all[F]
  }

  val oneOrTwoMaybe = {
    type F[A] = FreeT[OneOrTwo, Maybe, A]
    scalazlaws.monad.all[F]
  }
}
