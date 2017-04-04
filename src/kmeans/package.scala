import scala.collection.{GenMap, GenSeq}

package object kmeans {

  trait DataPoint {
    def apply(i: Int): Option[Double]
    def features: Int
    def keys: List[Int]
    def size: Int
    def norm: Double
    def computeNorm(xs: Iterable[Double]): Double =
      xs.foldLeft(0.0)((acc,v) => acc + math.pow(v,2))
  }

  class DenseDataPoint(val data: Vector[Double]) extends DataPoint {
    override def apply(i: Int): Option[Double] =
      if (data.isDefinedAt(i)) Some(data(i)) else None
    override val features: Int = data.length
    override val size: Int = features
    override lazy val keys: List[Int] = data.indices.toList
    override lazy val norm: Double = computeNorm(data)
  }

  class SparseDataPoint(val data: Map[Int, Double], val size: Int) extends DataPoint {
    override def apply(i: Int): Option[Double] = data.get(i)
    override val features: Int = data.size
    override lazy val keys: List[Int] = data.keys.toList
    override lazy val norm: Double = computeNorm(data.values)
  }

  object DataPoint {

    def apply(xs: Double*) = new DenseDataPoint(xs.toVector)
    def apply(xs: Vector[Double]) = new DenseDataPoint(xs)
    def apply(xs: Map[Int, Double], size: Int) = new SparseDataPoint(xs, size)

    def distance(p: DataPoint, q: DataPoint): Double = (p,q) match {
      case (w: DenseDataPoint, v: DenseDataPoint) =>
        require(w.size == v.size, "DataPoints must be of equal size")
        def go(i: Int, acc: Double): Double =
          if (i < 0) acc
          else go(i-1, acc + math.pow(w(i).getOrElse(0.0) - v(i).getOrElse(0.0), 2))
        go(p.size-1, 0.0)
      case (w, v) =>
        require(w.size == v.size, "DataPoints must be of equal size")
        val (big, small) = if (w.features > v.features) (w,v) else (v,w)
        def go(keys: List[Int], acc: Double): Double = keys match {
          case Nil => acc
          case h::t =>
            val sub = math.pow(big(h).getOrElse(0.0), 2)
            val add = math.pow(w(h).getOrElse(0.0) - v(h).getOrElse(0.0), 2)
            go(t, acc - sub + add)
        }
        big.norm + go(small.keys, 0.0)
    }

    def average(points: PointSeq): DataPoint = {
      require(points.forall(_.size == points.head.size), "DataPoints must be of equal size")
      val buf = Map.empty[Int, Double]
      if (points.isEmpty) DataPoint(buf, 0)
      else {
        def combine(acc: Map[Int,Double], point: DataPoint): Map[Int, Double] =
          acc ++ point.keys.map(i => i -> (point(i).get + acc.getOrElse(i, 0.0)))

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
