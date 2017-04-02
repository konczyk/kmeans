import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._
import KMeans._

@RunWith(classOf[JUnitRunner])
class KMeansTest extends FunSuite {

  def pt(xs: Double*) = DataPoint(xs.toVector)
  def data(x: DataPoint): Vector[Double] = x match {
    case dp: DenseDataPoint => dp.data
  }

  test("random centroids") {
    val seed = Some(5L)
    val c1 = pt(2,8)
    val c2 = pt(4,5)
    val c3 = pt(-9,2)
    val points: PointSeq = Seq(
      pt(0,0), c2, pt(2,3), pt(9,6), pt(8,2),
      c3, pt(8,6), pt(4,3), c1)

    val expected = Seq(c1, c2, c3)
    assert(randomCentroids(3, points, seed) === expected)
    assert(randomCentroids(3, points.par, seed) === expected)
  }

  test("k-means++ centroids") {
    val seed = Some(3L)
    val c1 = pt(-9,2)
    val c2 = pt(8,2)
    val c3 = pt(2,8)
    val points: PointSeq = Seq(
      pt(0,0), pt(4,5), pt(2,3), pt(9,6), c2,
      c1, pt(8,6), pt(4,3), c3)

    val expected = Seq(c1, c2, c3)
    assert(kppCentroids(3, points, seed) === expected)
    assert(kppCentroids(3, points.par, seed) === expected)
  }

  test("init random centroids") {
    val seed = Some(5L)
    val c1 = pt(2,8)
    val c2 = pt(4,5)
    val c3 = pt(-9,2)
    val points: PointSeq = Seq(
      pt(0,0), c2, pt(2,3), pt(9,6), pt(8,2),
      c3, pt(8,6), pt(4,3), c1)
    val kmeans = new KMeans(seed)

    val expected = Seq(c1, c2, c3)
    assert(kmeans.init(3, points, randomCentroids) === expected)
    assert(kmeans.init(3, points.par, randomCentroids) === expected)
  }

  test("init k-means++ centroids") {
    val seed = Some(3L)
    val c1 = pt(-9,2)
    val c2 = pt(8,2)
    val c3 = pt(2,8)
    val points: PointSeq = Seq(
      pt(0,0), pt(4,5), pt(2,3), pt(9,6), c2,
      c1, pt(8,6), pt(4,3), c3)
    val kmeans = new KMeans(seed)

    val expected = Seq(c1, c2, c3)
    assert(kmeans.init(3, points, kppCentroids) === expected)
    assert(kmeans.init(3, points.par, kppCentroids) === expected)
  }

  test("init with invalid number of clusters should throw exception") {
    val thrown = intercept[IllegalArgumentException] {
      new KMeans().init(0, Seq(), randomCentroids)
    }
    assert(thrown.getMessage contains "Number of clusters k")
  }

  test("assign data points to clusters") {
    val centroids: PointSeq = Seq(pt(1,4), pt(-30,7), pt(3,-2))
    val cluster1: PointSeq = Seq(
      pt(4,5), pt(2,3), pt(9,6), pt(-9,2),pt(8,6), pt(4,3), pt(2,8))
    val cluster2: PointSeq = Seq()
    val cluster3: PointSeq = Seq(pt(0,0), pt(8,2))
    val kmeans = new KMeans()

    val classification: PointMap = Map(
      centroids(0) -> cluster1,
      centroids(1) -> cluster2,
      centroids(2) -> cluster3)
    val points = cluster1 ++ cluster3
    assert(kmeans.classify(points, centroids) === classification)
    assert(kmeans.classify(points.par, centroids.par) === classification)
  }

  test("revise centroids") {
    val centroids: PointSeq = IndexedSeq(pt(1,4), pt(-30,7), pt(3,-2))
    val classified: PointMap = Map(
      centroids(0) -> Seq(
        pt(4,5), pt(2,3), pt(9,6), pt(-9,2), pt(8,6), pt(4,3), pt(2,8)),
      centroids(1) -> Seq(),
      centroids(2) -> Seq(pt(0,0), pt(8,2))
    )
    val kmeans = new KMeans()

    val expected = Seq(pt(20/7.0,33/7.0), pt(-30,7), pt(4,1)).map(data)
    val actual = kmeans.revise(classified, centroids).map(data)
    val actualPar = kmeans.revise(classified.par, centroids.par).map(data)
    assert(actual === expected)
    assert(actualPar === expected)
  }

  test("compute heterogeneity of the cluster assignments") {
    val centroids: PointSeq = IndexedSeq(pt(1,4), pt(-30,7), pt(3,-2))
    val classified: PointMap = Map(
      centroids(0) -> Seq(
        pt(4,5), pt(2,3), pt(9,6), pt(-9,2), pt(8,6), pt(4,3), pt(2,8)),
      centroids(1) -> Seq(),
      centroids(2) -> Seq(pt(0,0), pt(8,2))
    )
    val kmeans = new KMeans()

    assert(kmeans.heterogeneity(classified) === 318)
    assert(kmeans.heterogeneity(classified.par) === 318)
  }

  test("compute final centroids") {
    val centroids = IndexedSeq(pt(1,4), pt(-30,7), pt(3,-2))
    val points: PointSeq = Seq(
      pt(0,0), pt(4,5), pt(2,3), pt(9,6), pt(8,2),
      pt(-9,2), pt(8,6), pt(4,3), pt(2,8))
    val kmeans = new KMeans()

    val revised1 = Seq(pt(20/7.0,33/7.0), pt(-30,7), pt(4,1)).map(data)
    val actual1 = kmeans.kmeans(points, centroids, 10.0).map(data)
    val actualPar1 = kmeans.kmeans(points.par, centroids.par, 10.0).map(data)
    assert(actual1 === revised1)
    assert(actualPar1 === revised1)

    val revised2 = Seq(pt(16/6.0,30/6.0), pt(-30,7), pt(4,5/3.0)).map(data)
    val actual2 = kmeans.kmeans(points, centroids, 0.5).map(data)
    val actualPar2 = kmeans.kmeans(points.par, centroids.par, 0.5).map(data)
    assert(actual2 === revised2)
    assert(actualPar2 === revised2)
  }

}
