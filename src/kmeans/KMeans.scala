package kmeans

import scala.util.Random
import kmeans.DataPoint._

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
    go(Set(), Seq()).toIndexedSeq
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

  // sample a point using probability distribution, where probability
  // associated with each point is proportional to its distance from the nearest
  // centroid and the sampled point has the largest index such that P(X<=prob)
  private def probSample(points: PointSeq, centroids: PointSeq, prob: Double) = {
    val distances = points.map(
      p => centroids.foldLeft(Double.MaxValue)(_ min distance(p,_)))
    val total = distances.sum
    def go(idx: Int, acc: Double): Int =
      if (prob <= acc) idx
      else go(idx+ 1, acc + distances(idx+1)/total)

    go(0, distances.head/total)
  }

}

class KMeans(seed: Option[Long] = None) {

  def init(k: Int, points: PointSeq, initStrategy: CentroidStrategy): PointSeq = {
    require(k > 0, "Number of clusters k has to be larger than 0")
    initStrategy(k, points, seed)
  }

  def classify(points: PointSeq, centroids: PointSeq): PointMap =
    centroids.map(_ -> Seq()).toMap ++ points.groupBy(closestCentroid(centroids)(_))

  def closestCentroid(cs: PointSeq)(p: DataPoint): DataPoint = {
    def go(k: Int, minDist: Double, closest: DataPoint): DataPoint = {
      if (k < 0) closest
      else {
        val dist = distance(p, cs(k))
        if (dist < minDist) go(k-1, dist, cs(k))
        else go(k-1, minDist, closest)
      }
    }
    go(cs.length-1, distance(cs.head, p), cs.head)
  }

  // revise centroids based on the current cluster assignments
  def revise(classified: PointMap, centroids: PointSeq): PointSeq =
    centroids.map{c => classified.get(c) match {
      case Some(pts) if pts.nonEmpty => average(pts)
      case _ => c
    }}

  // compute the sum of all squared distances between data points and centroids
  def heterogeneity(classified: PointMap): Double =
    classified.map{case(k,xs) => xs.foldLeft(0.0)((acc,p) => acc + distance(p,k))}.sum

  final def kmeans(points: PointSeq, centroids: PointSeq, eta: Double, iter: Int): PointSeq = {
    val revisedCentroids = revise(classify(points, centroids), centroids)
    val converged = iter == 1 || (centroids zip revisedCentroids).forall {
      case (c1, c2) => distance(c1, c2) <= eta
    }
    if (converged) revisedCentroids
    else kmeans(points, revisedCentroids, eta, iter-1)
  }

}
