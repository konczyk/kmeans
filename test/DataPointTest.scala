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

  }

}
