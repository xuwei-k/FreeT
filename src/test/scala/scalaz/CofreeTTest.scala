package scalaz

import scalaz.std.anyVal._
import scalaprops._

object CofreeTTest extends Scalaprops {

  val maybeId = {
    type F[A] = CofreeT[Maybe, Id.Id, A]
    scalazlaws.comonad.all[F]
  }

  val iListId = {
    type F[A] = CofreeT[IList, Id.Id, A]
    scalazlaws.comonad.all[F]
  }.andThenParam(Param.maxSize(2))

}
