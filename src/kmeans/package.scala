import scala.collection.{GenMap, GenSeq}
import scala.util.Random

package object kmeans {

  case class DataPoint(features: GenSeq[Double]) {
    // return squared Euclidean distance between data points
    def distanceTo(that: DataPoint): Double = {
      features.zip(that.features).foldLeft(0.0) {
        case (acc, (a, b)) => acc + math.pow(b-a, 2)
      }
    }
  }

  type PointSeq = GenSeq[DataPoint]
  type PointMap = GenMap[DataPoint, PointSeq]
  type CentroidStrategy = (Int, PointSeq, Option[Long]) => PointSeq

}
