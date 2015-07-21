package scalaz

import scalaprops._
import scalaz.std.anyVal._

object FreeFTest extends Scalaprops {

  val maybe = scalazlaws.bifunctor.all[({type l[a, b] = FreeF[Maybe, a, b]})#l]
  val iList = scalazlaws.bifunctor.all[({type l[a, b] = FreeF[IList, a, b]})#l]

}
