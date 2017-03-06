import scala.collection.{GenMap, GenSeq}

package object kmeans {

  case class DataPoint(features: FeatureSeq) {
    // return squared Euclidean distance between data points
    def distanceTo(that: DataPoint): Double =
      features.zip(that.features).foldLeft(0.0) {
        case (acc, (a, b)) => acc + math.pow(b-a, 2)
      }
  }

  object DataPoint {

    def average(points: PointSeq): DataPoint = {
      val fts = points.map(_.features).reduce(_ zip _ map {case (a,b) => a+b})
      DataPoint(fts.map(_ / points.length))
    }

  }

  type FeatureSeq = GenSeq[Double]
  type PointSeq = GenSeq[DataPoint]
  type PointMap = GenMap[DataPoint, PointSeq]
  type CentroidStrategy = (Int, PointSeq, Option[Long]) => PointSeq

}
