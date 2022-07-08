package it.unibo.scafi

import com.github.tototoshi.csv.CSVReader
import it.unibo.scafi.space.{Point2D, Point3D}
import monocle.syntax.all._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object SensorTrace {
  // Type definitions
  final private case class SensorData(geoPosition: Point3D, data: Double)
  final private case class SpatioTemporalRecord(timestamp: Double, data: Seq[SensorData])
  private type SpatioTemporalData = Seq[SpatioTemporalRecord]
  // Constant
  private lazy val rainGaugeData = normalise(loadFromSource(Source.fromResource("toronto.csv")))
  def perceive(position: Point3D, timestamp: Double): Double =
    spatioTemporalSearch(timestamp, position, rainGaugeData)

  // Utility
  private def loadFromSource(input: Source): SpatioTemporalData =
    withCsvReader(CSVReader.open(input))(extractRainGaugeTemporalSeries)

  private def withCsvReader[K](reader: CSVReader)(map: CSVReader => K): K =
    try map(reader)
    finally reader.close()

  private def extractRainGaugeTemporalSeries(csv: CSVReader): SpatioTemporalData = {
    csv
      .all()
      .tail
      .map(marshallData)
      .collect { case Success(value) => value }
      .groupMap { case (k, v) => k } { case (k, v) => v }
      .toVector
      .map { case (when, data) => SpatioTemporalRecord(when, data) }
      .sortBy(_.timestamp)
  }

  private def marshallData(elements: List[String]): Try[(Double, SensorData)] = elements match {
    case entry :: id :: tpe :: date :: rainfall :: long :: lat :: seconds :: _ =>
      Try((seconds.toDouble) -> SensorData(Point2D(lat.toDouble, long.toDouble), rainfall.toDouble))
    case line =>
      Failure(new IllegalArgumentException(s"problems with csv file, line that throws the exception = $line"))
  }

  private def normalise(rainGaugeData: SpatioTemporalData): SpatioTemporalData = {
    val firstInstant = rainGaugeData.head.timestamp
    rainGaugeData.map(data => data.focus(_.timestamp).modify(_ - firstInstant))
  }

  private def spatioTemporalSearch(when: Double, where: Point3D, data: SpatioTemporalData): Double = {
    def spatialSearch(record: SpatioTemporalRecord): Double = {
      record.data.sortBy(_.geoPosition.distance(where)) match {
        case first :: second :: rest =>
          val totalDistance = first.geoPosition.distance(where) + second.geoPosition.distance(where)
          val ratio = first.geoPosition.distance(where) / totalDistance
          ((1 - ratio) * first.data + ratio * second.data) / 2
      }
    }
    @tailrec
    def binarySearch(boundLeft: Int, boundRight: Int): Double = if (boundLeft > boundRight) { //
      spatialSearch(data.last)
    } else {
      val centerIndex = (boundLeft + boundRight) / 2
      val center = data(centerIndex)
      if (center.timestamp == when) {
        spatialSearch(data(centerIndex))
      } else if (center.timestamp < when && centerIndex < data.size - 1 && data(centerIndex + 1).timestamp > when) { // in-between two data points
        // weighted average
        def timeOf(sample: Int) = data(sample).timestamp
        def dataOf(sample: Int) = spatialSearch(data(sample))
        val initialTime = timeOf(centerIndex)
        val finalTime = timeOf(centerIndex + 1)
        val timeSlice = finalTime - initialTime
        val rightBias = (when - initialTime) / timeSlice
        ((1 - rightBias) * dataOf(centerIndex) + rightBias * dataOf(centerIndex + 1))
      } else if (center.timestamp > when) {
        binarySearch(boundLeft, centerIndex - 1)
      } else {
        binarySearch(centerIndex + 1, boundRight)
      }
    }
    binarySearch(0, data.size - 1)
  }
}
