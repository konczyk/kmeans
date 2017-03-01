import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._
import KMeans._

@RunWith(classOf[JUnitRunner])
class KMeansTest extends FunSuite {

  val points = "0,0 4,5 2,3 9,6 8,2 -9,2 8,6 4,3 2,8"

  def parsePoints(pts: String): PointSeq =
    pts.split("\\s+").map(_.split(",").map(_.toDouble)).map(DataPoint(_))

  def pt(p: Double*) = DataPoint(p.toSeq)

  test("random centroids") {
    val pts = parsePoints(points)
    val seed = Some(5L)

    assert(randomCentroids(3, pts, seed) === Seq(pt(2,8), pt(4,5), pt(-9,2)))
  }

  test("kmeans++ centroids") {
    val pts = parsePoints(points)
    val seed = Some(3L)

    assert(kppCentroids(3, pts, seed) === Seq(pt(-9,2), pt(8,2), pt(2,8)))
  }


}