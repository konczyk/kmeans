import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._
import kmeans.DataPoint._

@RunWith(classOf[JUnitRunner])
class DataPointTest extends FunSuite {

  def pt(xs: Double*) = DataPoint(xs.toVector)

  test("distance between dense points of different size should throw exception") {
    val p1 = pt(1.0, 2)
    val p2 = pt(0.0, 2, -9, 2)
    val thrown = intercept[IllegalArgumentException] {
      distance(p1, p2)
    }
    assert(thrown.getMessage contains "DataPoints must be of equal size")
  }

  test("distance between dense points") {
    val p1 = pt(1.0, 2, 3, 4)
    val p2 = pt(0.0, 2, -9, 2)
    assert(distance(p1, p2) === 149)
  }

  test("average of list of dense points") {
    val p1 = pt(1.0, 2, 3, 4)
    val p2 = pt(0.0, 2, -9, 2)
    val p3 = pt(3.0, -2, 8, 2)

    val expected = Array(4/3.0, 2/3.0, 2/3.0, 8/3.0)
    assert(average(Seq(p1,p2,p3)).data === expected)
  }

}
