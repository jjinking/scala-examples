package swscala

import cats.{Contravariant, Functor, derive}
//import cats.syntax.functor._
//import example.Filterable._
//import org.scalacheck.ScalacheckShapeless._
import io.chymyst.ch._
import example.FilterableWithFilter

object Ch6 {

  object Part1 {

    object Problem1 {
      sealed trait Confucious[W]
      final case class ForgotAll[W]() extends Confucious[W]
      final case class ForgotFri[W](wSat: W) extends Confucious[W]
      final case class ForgotThu[W](wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotWed[W](wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotTue[W](wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotMon[W](wTue: W, wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotSun[W](wMon: W, wTue: W, wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class RememberAll[W](wSun: W, wMon: W, wTue: W, wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]

      implicit val functorConfucious = derive.functor[Confucious]

      implicit val filterableConfucious = new FilterableWithFilter[Confucious] {
        override def withFilter[A](p: A => Boolean)(fa: Confucious[A]): Confucious[A] = fa match {
          case ForgotAll() => ForgotAll()
          case ForgotFri(wSat) => if (!p(wSat)) ForgotAll() else fa
          case ForgotThu(wFri, wSat) => if (!p(wFri)) withFilter(p)(ForgotFri(wSat)) else fa
          case ForgotWed(wThu, wFri, wSat) => if (!p(wThu)) withFilter(p)(ForgotThu(wFri, wSat)) else fa
          case ForgotTue(wWed, wThu, wFri, wSat) => if (!p(wWed)) withFilter(p)(ForgotWed(wThu, wFri, wSat)) else fa
          case ForgotMon(wTue, wWed, wThu, wFri, wSat) => if (!p(wTue)) withFilter(p)(ForgotTue(wWed, wThu, wFri, wSat)) else fa
          case ForgotSun(wMon, wTue, wWed, wThu, wFri, wSat) => if (!p(wMon)) withFilter(p)(ForgotMon(wTue, wWed, wThu, wFri, wSat)) else fa
          case RememberAll(wSun, wMon, wTue, wWed, wThu, wFri, wSat) => if (!p(wSun)) withFilter(p)(ForgotSun(wMon, wTue, wWed, wThu, wFri, wSat)) else fa
        }
      }

      // Confucious is not a filterable functor because it violates the conjunction law
      // For example, if you have val c = RememberAll(1, 2, 4, 1, 2, 3, 6)
      // predicate 1: x > 3
      // predicate 2: x < 3
      // filter(p1) ◦ filter(p2) = ForgotMon(4, 1, 2, 3, 6) ◦ filter(p2) = ForgotTue(1, 2, 3, 6)
      // filter(p1 ◦ p2) = ForgotAll()
    }

    object Problem2 {

      implicit val functorIndexedSeq = new Functor[IndexedSeq] {
        override def map[A, B](fa: IndexedSeq[A])(f: A ⇒ B): IndexedSeq[B] = fa.map(f)
      }

      implicit val evenFilterableIndexedSeq = new FilterableWithFilter[IndexedSeq] {
        def evenFilter[A](p: A => Boolean)(fa: IndexedSeq[A]): IndexedSeq[A] = {
          if (fa.filter(t => !p(t)).length % 2 == 1) IndexedSeq() // Can we do this?
          else fa.filter(p)
        }

        override def withFilter[A](p: A => Boolean)(fa: IndexedSeq[A]): IndexedSeq[A] =
          evenFilter(p)(fa)
      }

      // IndexedSeq with evenFilter is not a filterable functor because it violates the conjunction law
      // Counterexample:
      // val s = IndexedSeq(1, 2, 3, 4, 5, 6)
      // predicate 1: x >= 2
      // predicate 2: x > 2
      // filter(p1) ◦ filter(p2) = IndexedSeq() ◦ filter(p2) = IndexedSeq()
      // filter(p1 ◦ p2) = IndexedSeq(3, 4, 5, 6)
    }

    // object Problem3 {
    //   F[A] ≡ Int + String × A × A × A

    //   // F[A] is not Not filterable since it is unclear what can be done to filter (String, A, A, A).
    //   // If any of the A elements do not pass the boolean function, we need to return Left(i:Int) but there is nowhere to get the integer value

    //   type F[A] = Either[Int, (String, A, A, A)]

    //   implicit val functorF = derive.functor[F]

    //   implicit val filterableF = new FilterableWithFilter[F] {
    //     override def withFilter[A](p: A => Boolean)(fa: F[A]): F[A] = fa match {
    //       case Left(i) => Left(i)
    //       case Right((_, a1, a2, a3)) if p(a1) && p(a2) && p(a3) => fa
    //       case _ => Left(i)
    //     }
    //   }
    // }

    object Problem4 {
      // final case class Q[A, Z](id: Long, user1: Option[(A, Z)], user2: Option[(A, Z)]) – with respect to the type parameter A
      final case class Q[A, Z](
        id: Long,
        user1: Option[(A, Z)],
        user2: Option[(A, Z)]
      )

      implicit def functorQ[Z] = new Functor[Q[?, Z]] {
        override def map[A, B](fa: Q[A, Z])(f: A ⇒ B): Q[B, Z] = {
          val user1New = fa.user1 match {
            case Some((a, z)) => Some((f(a), z))
            case _ => None
          }
          val user2New = fa.user2 match {
            case Some((a, z)) => Some((f(a), z))
            case _ => None
          }
          Q(fa.id, user1New, user2New)
        }
      }

      implicit def filterableQ[Z]: FilterableWithFilter[Q[?, Z]] = new FilterableWithFilter[Q[?, Z]]() {
        override def withFilter[A](p: A ⇒ Boolean)(fa: Q[A, Z]): Q[A, Z] = {
          val user1New = fa.user1 match {
            case Some((a, _)) if p(a) => fa.user1
            case _ => None
          }
          val user2New = fa.user2 match {
            case Some((a, _)) if p(a) => fa.user2
            case _ => None
          }
          Q(fa.id, user1New, user2New)
        }
      }
    }

    object Problem5 {
      // F[A] = MyTreeA defined recursively as F[A] ≡ 1 + A × F[A] × F[A]
      sealed trait MyTree[A]
      final case class MyTreeNil[A]() extends MyTree[A]
      final case class MyTreeNode[A](v: A, l: MyTree[A], r: MyTree[A]) extends MyTree[A]

      implicit val functorMyTree = derive.functor[MyTree]
      implicit val withFilterMyTree = new FilterableWithFilter[MyTree] {
        override def withFilter[A](p: A => Boolean)(fa: MyTree[A]): MyTree[A] = fa match {
          case MyTreeNode(v, l, r) if p(v) => MyTreeNode(v, withFilter(p)(l), withFilter(p)(r))
          case _ => MyTreeNil()
        }
      }
    }

    object Problem6 {

    }


  }

}
