import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._
import KMeans._

@RunWith(classOf[JUnitRunner])
class KMeansTest extends FunSuite {

  val points = "0,0 4,5 2,3 9,6 8,2 -9,2 8,6 4,3 2,8"
  val centroids = "2,8 4,5 -9,2"

  def parsePoints(pts: String): PointSeq =
    pts.split("\\s+").map(_.split(",").map(_.toDouble)).map(DataPoint(_))

  def pt(p: Double*) = DataPoint(p.toSeq)

  test("random centroids") {
    val pts = parsePoints(points)
    val seed = Some(5L)

    val expected = Seq(pt(2,8), pt(4,5), pt(-9,2))
    assert(randomCentroids(3, pts, seed) === expected)
    assert(randomCentroids(3, pts.par, seed) === expected)
  }

  test("k-means++ centroids") {
    val pts = parsePoints(points)
    val seed = Some(3L)

    val expected = Seq(pt(-9,2), pt(8,2), pt(2,8))
    assert(kppCentroids(3, pts, seed) === expected)
    assert(kppCentroids(3, pts.par, seed) === expected)
  }

  test("init random centroids") {
    val pts = parsePoints(points)
    val seed = Some(5L)
    val kmeans = new KMeans(seed)

    val expected = Seq(pt(2,8), pt(4,5), pt(-9,2))
    assert(kmeans.init(3, pts, randomCentroids) === expected)
    assert(kmeans.init(3, pts.par, randomCentroids) === expected)
  }

  test("init k-means++ centroids") {
    val pts = parsePoints(points)
    val seed = Some(3L)
    val kmeans = new KMeans(seed)

    val expected = Seq(pt(-9,2), pt(8,2), pt(2,8))
    assert(kmeans.init(3, pts, kppCentroids) === expected)
    assert(kmeans.init(3, pts.par, kppCentroids) === expected)
  }

  test("classify with non missing assignment") {
    val centroids = IndexedSeq(pt(1,4), pt(-3,7), pt(3,-2))
    val pts = parsePoints(points)
    val kmeans = new KMeans()

    val classification = Map(
      centroids(0) -> Seq(pt(4,5), pt(2,3), pt(9,6), pt(8,6), pt(4,3), pt(2,8)),
      centroids(1) -> Seq(pt(-9,2)),
      centroids(2) -> Seq(pt(0,0), pt(8,2))
    )
    assert(kmeans.classify(pts, centroids) === classification)
    assert(kmeans.classify(pts.par, centroids.par) === classification)
  }

  test("classify with missing assignment") {
    val centroids = IndexedSeq(pt(1,4), pt(-30,7), pt(3,-2))
    val pts = parsePoints(points)
    val kmeans = new KMeans()

    val classification = Map(
      centroids(0) -> Seq(pt(4,5), pt(2,3), pt(9,6), pt(-9,2),pt(8,6), pt(4,3), pt(2,8)),
      centroids(1) -> Seq(),
      centroids(2) -> Seq(pt(0,0), pt(8,2))
    )
    assert(kmeans.classify(pts, centroids) === classification)
    assert(kmeans.classify(pts.par, centroids.par) === classification)
  }

  test("revise with non missing assignment") {
    val centroids = IndexedSeq(pt(1,4), pt(-3,7), pt(3,-2))
    val classified= Map(
      centroids(0) -> Seq(pt(4,5), pt(2,3), pt(9,6), pt(8,6), pt(4,3), pt(2,8)),
      centroids(1) -> Seq(pt(-9,2)),
      centroids(2) -> Seq(pt(0,0), pt(8,2))
    )
    val kmeans = new KMeans()

    val expected = Seq(pt(29/6.0,31/6.0), pt(-9,2), pt(4,1))
    assert(kmeans.revise(classified, centroids) === expected)
    assert(kmeans.revise(classified.par, centroids.par) === expected)
  }

  test("revise with missing assignment") {
    val centroids = IndexedSeq(pt(1,4), pt(-30,7), pt(3,-2))
    val classified= Map(
      centroids(0) -> Seq(pt(4,5), pt(2,3), pt(9,6), pt(-9,2), pt(8,6), pt(4,3), pt(2,8)),
      centroids(1) -> Seq(),
      centroids(2) -> Seq(pt(0,0), pt(8,2))
    )
    val kmeans = new KMeans()

    val expected = Seq(pt(20/7.0,33/7.0), pt(-30,7), pt(4,1))
    assert(kmeans.revise(classified, centroids) === expected)
    assert(kmeans.revise(classified.par, centroids.par) === expected)
  }

}
