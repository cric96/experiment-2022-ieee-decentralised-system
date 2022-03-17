package it.unibo.geo.altitude

import it.unibo.geo.serialize.Representation._
import upickle.default._

import scala.io.Source
import scala.jdk.CollectionConverters.SeqHasAsJava
class AltitudeFromResource(val resource: String) extends AltitudeEvaluation {
  private val rawData = Source.fromResource("altitude.json").getLines().mkString
  private val altitudeData = read[List[Altitude]](rawData)
  private val position: java.util.List[Array[Double]] = altitudeData.map(_.geometry.coordinates.toArray).asJava
  private val altitude: Seq[java.lang.Double] = altitudeData.map(_.altitude)
  private val index = KDTreeFacade.createTree(position, altitude: _*)

  override def in(lat: Double, lon: Double): Double =
    index.nearest(Array(lon, lat)).value
}
