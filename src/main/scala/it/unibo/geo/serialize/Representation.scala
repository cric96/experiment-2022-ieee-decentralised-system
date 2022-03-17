package it.unibo.geo.serialize
import upickle.default.{ReadWriter => RW, macroRW}
object Representation {
  case class Altitude(altitude: Double, vertex: Boolean, geometry: Geometry)
  case class Geometry(`type`: String, coordinates: Seq[Double])
  implicit def geometryRW = macroRW[Geometry]
  implicit def altitudeRW = macroRW[Altitude]
}
