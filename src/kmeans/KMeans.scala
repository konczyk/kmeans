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

  // k-means++ algorithm
  def kppCentroids: CentroidStrategy = (k, pts, seed) => {
    val rnd = rand(seed)
    val first = rnd.nextInt(pts.length)
    def go(sample: PointSeq): PointSeq = {
      if (sample.size == k) sample
      else {
        val next = probSample(pts, sample, rnd.nextDouble)
        go(sample :+ pts(next))
      }
    }
    go(IndexedSeq(pts(first)))
  }

  // sample a point using probability mass function, where probability
  // associated with each point is proportional to its distance from the nearest
  // centroid and a random prob value
  private def probSample(points: PointSeq, centroids: PointSeq, prob: Double) = {
    val distances = points.map(
      p => centroids.foldLeft(Double.MaxValue)(_ min p.distanceTo(_))
    )
    val total = distances.sum
    def go(idx: Int, acc: Double): Int =
      if (prob <= acc) idx
      else go(idx+ 1, acc + distances(idx+1)/total)

    go(0, distances.head/total)
  }

}

class KMeans(seed: Option[Long] = None) {

  def init(k: Int, points: PointSeq, initStrategy: CentroidStrategy): PointSeq = {
    require(k > 0)
    initStrategy(k, points, seed)
  }

  def classify(points: PointSeq, centroids: PointSeq): PointMap =
    centroids.map(_ -> Seq()).toMap ++ points.groupBy(closestCentroid(_, centroids))

  private def closestCentroid(p: DataPoint, cs: PointSeq): DataPoint =
    cs.map(c => (c, c.distanceTo(p))).minBy(_._2)._1

  // revise centroids based on the current cluster assignments
  def revise(classified: PointMap, centroids: PointSeq): PointSeq =
    centroids.map{c => classified.get(c) match {
        case Some(pts) if pts.nonEmpty => DataPoint.average(pts)
        case _ => c
      }}

  // compute the sum of all squared distances between data points and centroids
  def heterogeneity(classified: PointMap): Double =
    (for (k <- classified.keys) yield classified(k).map(_ distanceTo k).sum).sum

}
