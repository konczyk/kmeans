package client

import java.io._
import java.net.URLConnection
import javax.imageio.ImageIO

import kmeans.KMeans._
import kmeans._
import org.rogach.scallop._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  banner("Usage: Client [options]")
  val sampling = opt[String](name = "sampling", short = 's',
    descr = "Sampling type: random | kmeans++ (default: kmeans++)",
    default = Some("kmeans++"),
    validate = (s: String) => s == "random" || s == "kmeans++")
  val clusters = opt[Int](name = "clusters", short = 'k', required = true,
    descr = "Number of clusters", validate = (i: Int) => i > 0)
  val runs = opt[Int](name = "runs", short = 'r', default = Some(1),
    descr = "Number of kmeans runs (default: 1)", validate = (i: Int) => i > 0)
  val src = trailArg[String](required = true, descr = "Input file")
  val help = opt[Boolean](name = "help", short = 'h', descr = "Usage help")
  verify()
}

object Client extends App {

  val conf = new Conf(args)

  val is = new BufferedInputStream(new FileInputStream(conf.src()))
  val contentType = Option(URLConnection.guessContentTypeFromStream(is))
  val isImage = contentType.getOrElse("").contains("image")
  val strategy = if (conf.sampling() == "random") randomCentroids else kppCentroids
  val eta = 1e-3
  val iter = 100

  if (isImage) clusterImage() else clusterText()

  private def computeClusters(kmeans: KMeans, points: PointSeq): PointSeq = {
    val results = for (_ <- 1 to conf.runs()) yield {
      val centroids = kmeans.init(conf.clusters(), points, strategy).par
      val clusters = kmeans.kmeans(points, centroids, eta, iter)
      val classified = kmeans.classify(points, clusters)
      (kmeans.heterogeneity(classified), clusters)
    }
    results.minBy(_._1)._2
  }

  private def clusterImage() = {
    import client.Image._

    val img = readImage(conf.src())
    val points = imageToDataPoints(img).par

    val kmeans = new KMeans()
    val bestClusters = computeClusters(kmeans, points)

    val newImg = dataPointsToImage(points, kmeans.closestCentroid(bestClusters), img.getWidth)
    ImageIO.write(newImg, "png", new BufferedOutputStream(System.out))
  }

  private def clusterText() = {
    import client.Text._

    val lines = readText(conf.src())
    val (points, wordMap) = textToDataPoints(lines, 0.005, 0.85)

    val kmeans = new KMeans()
    val parPoints = points.par
    val bestClusters = computeClusters(kmeans, parPoints)
    val classified = kmeans.classify(parPoints, bestClusters)

    for (i <- 0 until bestClusters.length) bestClusters(i) match {
      case (cluster: SparseDataPoint) =>
        val words = cluster.data.toSeq.sortBy(-_._2).take(10).map(t => wordMap(t._1))
        println(s"""
          |Cluster $i
          |Articles #:    ${classified(cluster).length}
          |Top 10 words:  ${words.mkString(", ")}""".stripMargin)
    }

  }

}
