package it.unibo.alchemist.loader.`export`.extractors

import it.unibo.alchemist.loader.`export`.exporters.AbstractExporter
import it.unibo.alchemist.model.interfaces
import it.unibo.alchemist.model.interfaces.{Environment, Position, Reaction}
import it.unibo.geo.altitude.AltitudeService
import it.unibo.scafi.SensorTrace
import it.unibo.scafi.space.Point3D
import upickle.default._

class RiskMap[T, P <: Position[P]](val path: String, val name: String, val samplingInterval: Double)
    extends AbstractExporter[T, P](samplingInterval: Double) {

  lazy val altitudesInToronto = AltitudeService.keyPointPosition.map { case (lat, lon) => AltitudeService.in(lat, lon) }
  lazy val min = altitudesInToronto.min
  lazy val max = altitudesInToronto.max
  lazy val altitudePercentage = altitudesInToronto.map(_ - min).map(identity[Double]).map(_ / max - min)
  override def exportData(
      environment: Environment[T, P],
      reaction: Reaction[T],
      time: interfaces.Time,
      l: Long
  ): Unit = {
    val riskMap = AltitudeService.keyPointPosition.map { case (lat, lon) =>
      (lat, lon, SensorTrace.perceive(Point3D(lat, lon, 0), l.toDouble), boundAltitude(AltitudeService.in(lat, lon)))
    }
    os.write(os.pwd / os.RelPath(path) / name / s"-$l", write(riskMap))
  }

  override def close(environment: Environment[T, P], time: interfaces.Time, l: Long): Unit = {}

  override def setup(environment: Environment[T, P]): Unit = {}

  private def boundAltitude(altitude: Double): Double = ((altitude - min) / (max - min)) + 1 // avoid 0 as denominator
}
object RiskMap {
  case class RiskLevel(lat: Double, lon: Double, risk: Double)
  implicit def macroRiskLevel: upickle.default.ReadWriter[RiskLevel] = macroRW
}
