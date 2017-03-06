import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import kmeans._

@RunWith(classOf[JUnitRunner])
class DataPointTest extends FunSuite {

  test("distanceTo returns squared Euclidean distance") {
    val p1 = Seq(1.0, 2, 3, 4)
    val p2 = Seq(0.0, 2, -9, 2)
    assert(DataPoint(p1).distanceTo(DataPoint(p2)) === 149)
    assert(DataPoint(p1.par).distanceTo(DataPoint(p2.par)) === 149)
  }

  test("average of list of points") {
    val p1 = DataPoint(Seq(1.0, 2, 3, 4))
    val p2 = DataPoint(Seq(0.0, 2, -9, 2))
    val p3 = DataPoint(Seq(3.0, -2, 8, 2))

    val expected = DataPoint(Seq(4/3.0, 2/3.0, 2/3.0, 8/3.0))
    assert(DataPoint.average(Seq(p1,p2,p3)) === expected)
  }

}
