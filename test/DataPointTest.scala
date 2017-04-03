import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._
import kmeans.DataPoint._

@RunWith(classOf[JUnitRunner])
class DataPointTest extends FunSuite {

  test("distance between dense points of different size should throw exception") {
    val p1 = DataPoint(1.0, 2)
    val p2 = DataPoint(0.0, 2, -9, 2)
    val thrown = intercept[IllegalArgumentException] {
      distance(p1, p2)
    }
    assert(thrown.getMessage contains "DataPoints must be of equal size")
  }

  test("distance between dense points") {
    val p1 = DataPoint(1.0, 2, 3, 4)
    val p2 = DataPoint(0.0, 2, -9, 2)
    assert(distance(p1, p2) === 149)
  }

  test("distance between sparse points") {
    val p1 = DataPoint(Map(0 -> 3.0, 3 -> 9.0), 4)
    val p2 = DataPoint(Map(0 -> 6.0, 2 -> 3.0), 4)
    assert(distance(p1, p2) === 99)
  }

  test("distance between mixed points") {
    val p1 = DataPoint(1.0, 2, 3, 4)
    val p2 = DataPoint(Map(0 -> 6.0, 2 -> 3.0), 4)
    assert(distance(p1, p2) === 45)
  }

  test("average of list of dense points") {
    val p1 = DataPoint(1.0, 2, 3, 4)
    val p2 = DataPoint(0.0, 2, -9, 2)
    val p3 = DataPoint(3.0, -2, 8, 2)

    val expected = Array(4/3.0, 2/3.0, 2/3.0, 8/3.0)
    val actual = average(Seq(p1,p2,p3))
    assert(actual.size === expected.length)
    assert(actual(0).get === expected(0))
    assert(actual(1).get === expected(1))
    assert(actual(2).get === expected(2))
    assert(actual(3).get === expected(3))
  }

  test("average of list of mixed points") {
    val p1 = DataPoint(1.0, 2, 3, 4)
    val p2 = DataPoint(Map(0 -> 8.0, 3 -> -8.0), 4)
    val p3 = DataPoint(3.0, -3, 8, 2)

    val expected = Array(12/3.0, -1/3.0, 11/3.0, -2/3.0)
    val actual = average(Seq(p1,p2,p3))
    assert(actual.size === expected.length)
    assert(actual(0).get === expected(0))
    assert(actual(1).get === expected(1))
    assert(actual(2).get === expected(2))
    assert(actual(3).get === expected(3))
  }

}
