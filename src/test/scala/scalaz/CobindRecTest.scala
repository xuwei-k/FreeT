package scalaz

import scalaprops._
import scalaz.std.anyVal._

object CobindRecTest extends Scalaprops{

  val cobindRecLaw = Property.forAll[NonEmptyList[Byte], NonEmptyList[Byte] => Byte]{
    (nel, nat) =>
      CobindRec[NonEmptyList].laws.consistency(nel, nat)
  }

}
