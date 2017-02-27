package kmeans

import scala.util.Random

object KMeans {

  private def rand(seed: Option[Long]) = seed match {
    case None => new Random()
    case Some(v) => new Random(v)
  }

  def randomCentroids: CentroidStrategy = (k, pts, seed) => {
    val rnd = rand(seed)
    def go(seen: Set[Int], sample: PointSeq): PointSeq = {
      if (sample.size == k) sample
      else {
        val next = rnd.nextInt(pts.length)
        if (!seen.contains(next)) go(seen + next, sample :+ pts(next))
        else go(seen, sample)
      }
    }
    go(Set(), IndexedSeq())
  }

}

class KMeans(seed: Option[Long] = None) {

  def init(k: Int, points: PointSeq, initStrategy: CentroidStrategy): PointSeq =
    initStrategy(k, points, seed)

}
