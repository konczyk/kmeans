import scala.collection.{GenMap, GenSeq}

package object kmeans {

  trait DataPoint {
    def size: Int
  }

  class DenseDataPoint(val data: Vector[Double]) extends DataPoint {
    override def size: Int = data.length
  }

  object DenseDataPoint {
    def unapply(point: DenseDataPoint): Option[Vector[Double]] = Some(point.data)
  }

  object DataPoint {

    def apply(xs: Double*): DenseDataPoint = new DenseDataPoint(xs.toVector)
    def apply(xs: Vector[Double]): DenseDataPoint = new DenseDataPoint(xs)

    def distance(p: DataPoint, q: DataPoint): Double = {
      require(p.size == q.size, "DataPoints must be of equal size")
      (p, q) match {
        case (DenseDataPoint(pd), DenseDataPoint(qd)) =>
          def go(i: Int, acc: Double): Double = {
            if (i < 0) acc
            else go(i - 1, acc + math.pow(pd(i) - qd(i), 2))
          }
          go(p.size-1, 0)
        case _ => 0
      }
    }

    def average(points: PointSeq): DenseDataPoint = {
      val buf = Vector.fill[Double](points.head.size)(0)
      if (points.isEmpty) DataPoint(buf)
      else {
        def combine(acc: Vector[Double], point: DataPoint): Vector[Double] =
          point match {
            case DenseDataPoint(data) => reduce(acc, data)
          }
        def reduce(p: Vector[Double], q: Vector[Double]): Vector[Double] = {
          val buf = Array.fill[Double](p.size)(0)
          p.indices.foreach(i => buf(i) = p(i) + q(i))
          buf.toVector
        }

        val summed = points.aggregate(buf)(combine, reduce)
        DataPoint(summed.map(_ / points.length))
      }
    }
  }

  type PointSeq = GenSeq[DataPoint]
  type PointMap = GenMap[DataPoint, PointSeq]
  type CentroidStrategy = (Int, PointSeq, Option[Long]) => PointSeq

}
