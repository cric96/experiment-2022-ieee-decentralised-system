package it.unibo.alchemist.loader.export.exporters

import it.unibo.alchemist.model.interfaces
import it.unibo.alchemist.model.interfaces.{Environment, Position, Reaction}
import it.unibo.geo.altitude.AltitudeService
import it.unibo.scafi.SensorTrace
import it.unibo.scafi.space.Point3D
import upickle.default._
class RiskMap[T, P <: Position[P]](val path: String, val name: String, val samplingInterval: Double)
    extends AbstractExporter[T, P](samplingInterval: Double) {
  private lazy val altitudesInToronto = AltitudeService.keyPointPosition.map { case (lat, lon) =>
    AltitudeService.in(lat, lon)
  }
  private lazy val min = altitudesInToronto.min
  private lazy val max = altitudesInToronto.max
  private var riskSnapshotData: Map[String, Seq[RiskMapInfo.RiskSnapshot]] = Map.empty
  override def exportData(
      environment: Environment[T, P],
      reaction: Reaction[T],
      time: interfaces.Time,
      l: Long
  ): Unit = {
    val riskMap = AltitudeService.keyPointPosition.map { case (lat, lon) =>
      RiskMapInfo.RiskLevel(
        lat,
        lon,
        SensorTrace.perceive(Point3D(lat, lon, 0), time.toDouble) / boundAltitude(AltitudeService.in(lat, lon))
      )
    }
    val snapshot = RiskMapInfo.RiskSnapshot(time.toDouble.toLong, riskMap)
    riskSnapshotData = riskSnapshotData + (variablesDescriptor -> (riskSnapshotData.getOrElse(
      variablesDescriptor,
      Seq.empty
    ) :+ snapshot))
  }

  override def close(environment: Environment[T, P], time: interfaces.Time, l: Long): Unit = {
    val allSnapshots = riskSnapshotData(variablesDescriptor)
    os.write.over(os.pwd / os.RelPath(path) / s"$name-$variablesDescriptor", write(allSnapshots))
    riskSnapshotData -= variablesDescriptor
  }

  override def setup(environment: Environment[T, P]): Unit =
    os.makeDir.all(os.pwd / os.RelPath(path))

  private def boundAltitude(altitude: Double): Double =
    ((altitude - min) / (max - min)) + 1 // avoid 0 as denominator

}
