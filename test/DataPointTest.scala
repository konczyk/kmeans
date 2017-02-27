import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._

@RunWith(classOf[JUnitRunner])
class DataPointTest extends FunSuite {

  test("distanceTo returns squared Euclidean distance") {
    val p1 = DataPoint(List(1, 2, 3, 4))
    val p2 = DataPoint(List(0, 2, -9, 2))
    assert(p1.distanceTo(p2) === 149)
  }

  test("distanceTo returns squared Euclidean distance for parallel seq") {
    val p1 = DataPoint(List(1.0, 2.0, 3.0, 4.0).par)
    val p2 = DataPoint(List(0.0, 2.0, -9.0, 2.0).par)
    assert(p1.distanceTo(p2) === 149)
  }

}
