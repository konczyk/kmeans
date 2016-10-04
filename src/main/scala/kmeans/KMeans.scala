package kmeans

import scala.collection.GenMap
import scala.collection.GenSeq

class KMeans() {

  def init(
    points: GenSeq[DataPoint],
    k: GenSeq[DataPoint]
  ): GenSeq[DataPoint] = ???

  def assign(
    points: GenSeq[DataPoint],
    centroids: GenSeq[DataPoint]
  ): GenMap[DataPoint, GenSeq[DataPoint]] = ???

  def train(
    data: GenSeq[DataPoint],
    k: Int,
    maxIter: Int
  ): GenSeq[DataPoint] = ???

}
