package it.unibo.scafi

import com.github.tototoshi.csv.CSVReader
import it.unibo.scafi.space.{Point2D, Point3D}
import monocle.syntax.all._

import java.text.SimpleDateFormat
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object SensorTrace {
  // Type definitions
  final private case class SensorData(geoPosition: Point3D, data: Double)
  final private case class SpatioTemporalRecord(when: Double, data: Seq[SensorData])
  private type SpatioTemporalData = Seq[SpatioTemporalRecord]
  // Constant
  private val rainGaugeData = normalise(loadFromSource(Source.fromResource("toronto.csv")))

  def perceive(where: Point3D, at: Double): Double =
    spatialSearch(where, temporalSearch(at, rainGaugeData))

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
      .tapEach { case Failure(exception) => println(exception.getMessage + exception.getCause); case _ => } // TODO fix
      .collect { case Success(value) => value }
      .groupMap { case (k, v) => k } { case (k, v) => v }
      .toVector
      .map { case (when, data) => SpatioTemporalRecord(when, data) }
      .sortBy(_.when)(Ordering[Double].reverse)
  }

  private def marshallData(elements: List[String]): Try[(Double, SensorData)] = elements match {
    case l @ entry :: id :: tpe :: date :: rainfall :: long :: lat :: seconds :: _ =>
      Try(seconds.toDouble -> SensorData(Point2D(long.toDouble, lat.toDouble), rainfall.toDouble))
    case line =>
      Failure(new IllegalArgumentException(s"problems with csv file, line that throws the exception = $line"))
  }

  private def normalise(rainGaugeData: SpatioTemporalData): SpatioTemporalData = {
    val firstInstant = rainGaugeData.head.when
    rainGaugeData.map(data => data.focus(_.when).modify(_ - firstInstant))
  }

  private def temporalSearch(when: Double, data: SpatioTemporalData): SpatioTemporalRecord = {
    @tailrec
    def binarySearch(boundLeft: Int, boundRight: Int): SpatioTemporalRecord = if (boundRight > boundLeft) { //
      data.last
    } else {
      val centerIndex = (boundLeft + boundRight) / 2
      val center = data(centerIndex)
      if (center.when == when || center.when < when && centerIndex > 0 && data(centerIndex - 1).when > when) {
        data(centerIndex - 1)
      } else if (center.when > when) {
        binarySearch(boundLeft, centerIndex - 1)
      } else {
        binarySearch(centerIndex + 1, boundRight)
      }
    }
    binarySearch(0, data.size)
  }

  private def spatialSearch(where: Point3D, record: SpatioTemporalRecord): Double =
    record.data.minBy(_.geoPosition.distance(where)).data
}
