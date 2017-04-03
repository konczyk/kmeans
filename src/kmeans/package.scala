import scala.collection.{GenMap, GenSeq}

package object kmeans {

  trait DataPoint {
    def size: Int
    def apply(i: Int): Option[Double]
    def indices: Iterable[Int]
  }

  class DenseDataPoint(val data: Vector[Double]) extends DataPoint {
    override def size: Int = data.length
    override def apply(i: Int): Option[Double] = Some(data(i))
    override def indices: Iterable[Int] = data.indices
  }

  class SparseDataPoint(val data: Map[Int, Double], val size: Int) extends DataPoint {
    override def apply(i: Int): Option[Double] = data.get(i)
    override def indices: Iterable[Int] = data.keys
  }

  object DataPoint {

    def apply(xs: Double*) = new DenseDataPoint(xs.toVector)
    def apply(xs: Vector[Double]) = new DenseDataPoint(xs)
    def apply(xs: Map[Int, Double], size: Int) = new SparseDataPoint(xs, size)

    def distance(p: DataPoint, q: DataPoint): Double = {
      require(p.size == q.size, "DataPoints must be of equal size")
      def go(i: Int, acc: Double): Double = {
        if (i < 0) acc
        else (p(i), q(i)) match {
          case (None, None) =>
            go(i-1, acc)
          case (v, w) =>
            go(i-1, acc + math.pow(v.getOrElse(0.0) - w.getOrElse(0.0), 2))
        }
      }
      go(p.size-1, 0)
    }

    def average(points: PointSeq): DataPoint = {
      require(points.forall(_.size == points.head.size), "DataPoints must be of equal size")
      val buf = Map.empty[Int, Double]
      if (points.isEmpty) DataPoint(buf, 0)
      else {
        def combine(acc: Map[Int,Double], point: DataPoint): Map[Int, Double] =
          acc ++ point.indices.map(i => i -> (point(i).get + acc.getOrElse(i, 0.0)))

        def reduce(p: Map[Int, Double], q: Map[Int, Double]): Map[Int, Double] =
          p ++ q.map{case (k,v) => k -> (v + p.getOrElse(k, 0.0))}

        val summed = points.aggregate(buf)(combine, reduce)
        DataPoint(summed.map{case (k,v) => k -> v / points.length}, points.head.size)
      }
    }
  }

  type PointSeq = GenSeq[DataPoint]
  type PointMap = GenMap[DataPoint, PointSeq]
  type CentroidStrategy = (Int, PointSeq, Option[Long]) => PointSeq

}
