package swscala.unit

import cats.syntax.functor._
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{Assertion, FlatSpec}

import example.Filterable._
import example.{ContraFilterableWithFilter, ContraFilterableLawChecking, FilterableLawChecking}

import swscala._

class Ch6Spec extends FlatSpec with FilterableLawChecking with ContraFilterableLawChecking {

  import Ch6._

  behavior of "Exercises 1"

  it should "Problem 1" in {
    import Part1.Problem1._

    val data: Confucious[String] = ForgotSun("mon", "tue", "wed", "thu", "fri", "sat")

    val result: Confucious[String] = for {
      x ← data
      if x == "thu" // forget before thu
      y = s"$x drink"
    } yield y

    result shouldEqual ForgotWed("thu drink", "fri drink", "sat drink")

    // Fails
    //checkFilterableLawsWithFilter[Confucious, Boolean, Int]()
  }

  it should "Problem 2" in {
    import Part1.Problem2._

    evenFilterableIndexedSeq.evenFilter[Int](_ > 3)(IndexedSeq(0, 1, 2, 3, 4, 5)) shouldEqual IndexedSeq(4, 5)
    evenFilterableIndexedSeq.evenFilter[Int](_ > 3)(IndexedSeq(1, 2, 3, 4, 5)) shouldEqual IndexedSeq()

    // Fails
    // checkFilterableLawsWithFilter[IndexedSeq, Int, Boolean]()
    // checkFilterableLawsWithFilter[IndexedSeq, String, Double]()

    // Counterexample:
    val s = IndexedSeq(1, 2, 3, 4, 5, 6)
    def p1(x: Int): Boolean = x >= 2
    def p2(x: Int): Boolean = x > 2
    // def p12(x: Int): Boolean = p1(x) && p2(x)
//     val r1 = for {
//       x <- s
//       if p1(x)
// //      if p2(x)
//     } yield x
//     r1 shouldEqual IndexedSeq()

//     // val r2 = for {
//     //   x <- s
//     //   if p12(x)
//     // } yield x
//     // r1 should not equal r2
    // doesn't work probably because filter syntax is already implemented for IndexedSeq

    val r1 = evenFilterableIndexedSeq.evenFilter(p1)(s)
    r1 shouldEqual IndexedSeq()
    val r2 = evenFilterableIndexedSeq.evenFilter(p2)(r1)
    r2 shouldEqual IndexedSeq()
    val r3 = evenFilterableIndexedSeq.evenFilter((x: Int) => p1(x) && p2(x))(s)
    r3 shouldEqual IndexedSeq(3, 4, 5, 6)
    r2 should not equal r3
  }

  it should "Problem 4" in {
    import Part1.Problem4._

    checkFilterableLawsWithFilter[Q[?, Boolean], Int, Boolean]()
    checkFilterableLawsWithFilter[Q[?, Int], String, Double]()
  }

  it should "Problem 5" in {
    import Part1.Problem5._

    checkFilterableLawsWithFilter[MyTree, Int, Boolean]()
    checkFilterableLawsWithFilter[MyTree, String, Double]()
  }

  // it should "Problem 6" in {
  //   import Part1.Problem6._

  //   checkFilterableLawsWithFilter[R, Int, Boolean]()
  //   checkFilterableLawsWithFilter[R, String, Double]()
  // }

  it should "Problem 7" in {
    import Part1.Problem7._

    type Example7[A] = C[A, String]

    // Now have type class instance for Example7
    implicitly[ContraFilterableWithFilter[Example7]]

    // Define equal for Example7
    def e7Equal[A: Arbitrary](x: Example7[A], y: Example7[A]): Assertion =
      forAll { (a1: A, a2: A, a3: A) ⇒ x(a1, a2, a3) shouldEqual y(a1, a2, a3) }

    checkContraFilterableLawsWithFilter[Example7, Int, String](e7Equal)
  }

}
