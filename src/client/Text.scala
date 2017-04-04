package client

import scala.collection.mutable

import kmeans._

object Text {

  def readText(path: String): Iterator[String] =
    scala.io.Source.fromFile(path).getLines

  def textToDataPoints(lines: Iterator[String], minDf: Double, maxDf: Double):
      (PointSeq, Vector[String]) = {
    // word -> (idx, doc freq)
    val wordMap = mutable.Map[String, (Int,Int)]()
    val docs = mutable.ArrayBuffer[Map[String, Int]]()

    lines.foreach(line => {
      val terms = tf(line.split(",")(1))
      docs += terms
      terms.keysIterator.foreach(term => wordMap.get(term) match {
        case None => wordMap(term) = (wordMap.size, 1)
        case Some(v) => wordMap(term) = (v._1, v._2 + 1)
      })
    })

    // remove words that appear in less than min documents or more than max
    val min = (minDf * docs.length).toInt
    val max = (maxDf * docs.length).toInt
    wordMap --= wordMap.filter{case (_,v) => v._2 < min || v._2 > max}.keys
    val keys = wordMap.keys.toArray
    for (i <- keys.indices) wordMap(keys(i)) = (i, wordMap(keys(i))._2)

    def idf(word: String) = math.log(docs.length / wordMap(word)._2.toDouble)

    val points = docs.map(terms => {
      val tf_idf = terms
        .filter(wordMap isDefinedAt _._1)
        .map{case (t,tf) => t -> tf * idf(t)}
      val norm = normalize(tf_idf)
      DataPoint(tf_idf.map{case (k,v) => wordMap(k)._1 -> v/norm}, wordMap.size)
    })
    val terms = wordMap.toSeq.sortBy(_._2._1).map(_._1)

    (points.toVector, terms.toVector)
  }

  private def normalize(terms: Map[String,Double]) =
    math.sqrt(terms.values.foldLeft(0.0)(_ + math.pow(_,2)))

  private def tf(line: String): Map[String, Int] = {
    val terms = mutable.Map[String,Int]()
    line.split("\\s+").foreach(term =>
      if (terms.contains(term)) terms(term) += 1
      else terms(term) = 1
    )
    terms.toMap
  }

}
