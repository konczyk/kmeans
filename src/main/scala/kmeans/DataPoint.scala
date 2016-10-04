package kmeans

import scala.collection.GenSeq

/**
  * A data point in the n-dimensional space.
  */
case class DataPoint(features: GenSeq[Double])
