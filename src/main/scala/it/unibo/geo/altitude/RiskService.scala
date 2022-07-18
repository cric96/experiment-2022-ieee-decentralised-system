package it.unibo.geo.altitude

import it.unibo.alchemist.loader.`export`.exporters.RiskMapInfo
import it.unibo.alchemist.loader.`export`.exporters.RiskMapInfo.RiskLevel
import it.unibo.alchemist.model.interfaces.{Environment, Node, Position, Time}
import it.unibo.scafi.SensorTrace
import it.unibo.scafi.space.Point3D

object RiskService {
  private lazy val altitudesInToronto = AltitudeService.keyPointPosition.map { case (lat, lon) =>
    AltitudeService.in(lat, lon)
  }
  private lazy val min = altitudesInToronto.min
  private lazy val max = altitudesInToronto.max

  def riskMapAt(time: Time): Iterable[RiskLevel] = AltitudeService.keyPointPosition.map { case (lat, lon) =>
    RiskMapInfo.RiskLevel(
      lat,
      lon,
      SensorTrace.perceive(Point3D(lat, lon, 0), time.toDouble) / boundAltitude(AltitudeService.in(lat, lon))
    )
  }

  def evalRiskOf(node: Node[Any], environment: Environment[Any, Position[_]]): Double = {
    val position = environment.getPosition(node)
    val (lat, lon) = (position.getCoordinate(0), position.getCoordinate(1))
    SensorTrace.perceive(Point3D(lat, lon, 0), environment.getSimulation.getTime.toDouble) / boundAltitude(
      AltitudeService.in(lat, lon)
    )
  }

  private def boundAltitude(altitude: Double): Double =
    ((altitude - min) / (max - min)) + 1 // avoid 0 as denominator

}
