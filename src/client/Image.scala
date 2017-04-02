package client

import java.awt.image.BufferedImage
import java.io._
import javax.imageio._

import kmeans._

object Image {

  def readImage(path: String): BufferedImage = ImageIO.read(new File(path))

  def imageToDataPoints(image: BufferedImage): PointSeq = {
    val width = image.getWidth
    val height = image.getHeight
    val buf = new Array[DataPoint](width*height)

    buf.indices.foreach(i => {
      val x = i % width
      val y = i / width
      buf(i) = colorToDataPoint(image.getRGB(x, y))
    })

    buf.toIndexedSeq
  }

  private def colorToDataPoint(rgb: Int): DataPoint =
    DataPoint(
      ((rgb >> 16) & 0xff).toDouble / 256,
      ((rgb >> 8) & 0xff).toDouble / 256,
      (rgb & 0xff).toDouble / 256
    )

  def dataPointsToImage(points: PointSeq, classifier: DataPoint => DataPoint, width: Int): BufferedImage = {
    val image = new BufferedImage(width, points.length / width, BufferedImage.TYPE_INT_RGB)
    (0 until points.length).foreach(i => {
      val x = i % width
      val y = i / width
      val cluster = classifier(points(i))
      image.setRGB(x, y, dataPointToColor(cluster))
    })
    image
  }

  private def dataPointToColor(p: DataPoint): Int = p match {
    case DenseDataPoint(data) =>
      val colors = data.map(_ * 256).map(_.toInt)
      (colors(0) << 16) | (colors(1) << 8) | colors(2)
  }
}
