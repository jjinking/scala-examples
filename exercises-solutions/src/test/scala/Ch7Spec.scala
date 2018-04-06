package swscala.unit

import org.scalatest._

import swscala._

class Ch7Spec extends FlatSpec with Matchers {

  import Ch7._

  behavior of "Exercises 1"

  it should "Problem 1" in {
    import Part1.Problem1._

    val nums = Set(1, 2, 3, 4)
    findQuad(nums) shouldEqual Set(Set(1, 2, 3, 4))

    val nums2 = Set(1, 2, 3, 5)
    findQuad(nums2) shouldEqual Set.empty

    val nums3 = Set(1, 2, 3, 6, 7, 8)
    findQuad(nums3) shouldEqual Set(
      Set(1, 2, 6, 7),
      Set(1, 2, 7, 8),
      Set(1, 3, 6, 8),
      Set(2, 3, 6, 7),
      Set(2, 3, 7, 8))

  }

  it should "Problem 2" in {
    import Part1.Problem2._

    findTriplets(
      Seq(1,2,3),
      Seq(2,3,4),
      Seq(3,4,10)
    ) shouldEqual Seq(
      (1, 2, 3),
      (1, 2, 4),
      (1, 3, 4),
      (2, 3, 4))
  }


}
