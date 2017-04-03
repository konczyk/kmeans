package client

import java.io._
import java.net.URLConnection
import javax.imageio.ImageIO

import client.Image._
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
  val isImage = URLConnection.guessContentTypeFromStream(is).contains("image")
  val strategy = if (conf.sampling() == "random") randomCentroids else kppCentroids
  val eta = 0.001

  if (isImage) clusterImage()

  private def computeClusters(kmeans: KMeans, points: PointSeq): PointSeq = {
    val results = for (_ <- 1 to conf.runs()) yield {
      val centroids = kmeans.init(conf.clusters(), points, strategy).par
      val clusters = kmeans.kmeans(points, centroids, eta)
      val classified = kmeans.classify(points, clusters)
      (kmeans.heterogeneity(classified), clusters)
    }
    results.minBy(_._1)._2
  }

  private def clusterImage() = {
    val img = readImage(conf.src())
    val points = imageToDataPoints(img).par

    val kmeans = new KMeans()
    val bestClusters = computeClusters(kmeans, points)

    val newImg = dataPointsToImage(points, kmeans.closestCentroid(bestClusters), img.getWidth)
    ImageIO.write(newImg, "png", new BufferedOutputStream(System.out))
  }

}
