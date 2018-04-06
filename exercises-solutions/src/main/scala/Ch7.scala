package swscala

object Ch7 {

  object Part1 {

    object Problem1 {

      def findQuad(nums: Set[Int]): Set[Set[Int]] = {
        {
          for {
            ws <- nums.toList.sorted.tails
            if ws.nonEmpty
            w = ws.head
            rest1 = ws.tail
            xs <- rest1.tails
            if xs.nonEmpty
            x = xs.head
            rest2 = xs.tail
            ys <- rest2.tails
            if ys.nonEmpty
            y = ys.head
            rest3 = ys.tail
            zs <- rest3.tails
            if zs.nonEmpty
            z = zs.head
            if w + z == x + y
          } yield Set(w, x, y, z)
        }.toSet
      }

    }

    object Problem2 {

      def findTriplets(xs: Seq[Int], ys: Seq[Int], zs: Seq[Int]): Seq[(Int, Int, Int)] = {
        for {
          x <- xs
          y <- ys
          if x < y && x + y < 9
          z <- zs
          if y < z && x + y + z < 10
        } yield (x, y, z)
      }

    }


  }


}
