package scalaz

trait CobindRec[F[_]] extends Cobind[F] {
  def cobindRec[A, B](f: F[A] => A \/ B)(fa: F[A]): F[B]

  trait CobindRecLaws extends CobindLaws {
    def consistency[A, B](a: F[A], f: F[A] => B)(implicit E: Equal[F[B]]) = {
      val z = cobindRec[A, B]{ fa =>
        \/-(f(fa))
      }(a)

      val x = E.equal(cobind(a)(f), z)

      if(!x){
        println((cobind(a)(f), z).toString)
      }
      x
    }
  }

  def laws = new CobindRecLaws {}
}

object Main {
  def main(args: Array[String]): Unit = {
    import std.anyVal._
    val a = NonEmptyList(1,2,3)
    val F = CobindRec[NonEmptyList]

    val z: NonEmptyList[Int] = F.cobindRec[Int, Int]{
      x =>
        if(x.head > 1){
          \/-(x.head)
        }else{
          \/-(9)
        }
    }(a)

    val p: NonEmptyList[Int] = F.cobind(a){
      x =>
        if(x.head > 1)
          x.head
        else
          9
    }

    import syntax.equal._

    println(z === p)

    val test = F.laws.consistency[Int, Int](a, _.head)
    println(test)

  }
}

object CobindRec {
  def apply[F[_]](implicit F: CobindRec[F]): CobindRec[F] = F

  implicit val nel: CobindRec[NonEmptyList] = new CobindRec[NonEmptyList] {
    override def cobindRec[A, B](f: NonEmptyList[A] => A \/ B)(fa: NonEmptyList[A]) = {
      @annotation.tailrec
      def go(fa0: NonEmptyList[A], xs: IList[B], i: Int): NonEmptyList[B] = {
        if(i > 100){
          sys.error("100")
        }
        f(fa0) match {
          case \/-(b) =>
            fa0.tail.toNel match {
              case Some(x) =>
                go(x, b :: xs, i + 1)
              case None =>
                NonEmptyList.nel(b, xs).reverse
            }
          case -\/(a) =>
            go(a <:: fa0, xs, i + 1)
        }
      }
      go(fa, IList.empty, 0)
    }


    override def cobind[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B) =
      NonEmptyList.nonEmptyList.cobind(fa)(f)

    override def map[A, B](fa: NonEmptyList[A])(f: A => B) = fa map f
  }
}
