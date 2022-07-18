package it.unibo.alchemist.loader.export.exporters

import it.unibo.alchemist.model.interfaces
import it.unibo.alchemist.model.interfaces.{Environment, Position, Reaction}
import it.unibo.geo.altitude.{AltitudeService, RiskService}
import upickle.default._
class RiskMap[T, P <: Position[P]](val path: String, val name: String, val samplingInterval: Double)
    extends AbstractExporter[T, P](samplingInterval: Double) {

  private var riskSnapshotData: Map[String, Seq[RiskMapInfo.RiskSnapshot]] = Map.empty
  override def exportData(
      environment: Environment[T, P],
      reaction: Reaction[T],
      time: interfaces.Time,
      l: Long
  ): Unit = {
    val riskMap = RiskService.riskMapAt(time)
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
}
