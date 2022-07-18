package it.unibo.alchemist.loader.`export`.exporters

import it.unibo.alchemist.loader.`export`.ExportUtil
import it.unibo.alchemist.loader.`export`.exporters.FireStationPosition.FireStationSnapshot
import it.unibo.alchemist.model.interfaces.{Environment, Position, Reaction, Time}
import upickle.default._

class FireStationPosition[T, P <: Position[P]](val path: String, val name: String, val samplingInterval: Double)
    extends AbstractExporter[T, P](samplingInterval: Double) {

  var positionForExperiment: Map[String, Seq[FireStationSnapshot]] = Map.empty
  override def exportData(environment: Environment[T, P], reaction: Reaction[T], time: Time, l: Long): Unit = {
    val fireStation = ExportUtil.getFireStationsFromEnvironment(environment)
    val stationAndAlarm = fireStation
      .map(node => (node, node.get[Int]("solve-id")))
      .filter { case (_, alarm) => alarm > 0 }
      .groupMapReduce(_._2)(node => Set(node._1))(_ ++ _)

    val stationsInAlarm = stationAndAlarm.values.flatten
      .map(node => environment.getPosition(node.node))
      .map(position => (position.getCoordinate(0), position.getCoordinate(1)))

    val snapshot = FireStationSnapshot(time.toDouble.toLong, stationsInAlarm)
    positionForExperiment += variablesDescriptor -> (positionForExperiment.getOrElse(
      variablesDescriptor,
      Seq.empty
    ) :+ snapshot)
  }

  override def close(environment: Environment[T, P], time: Time, l: Long): Unit = {
    val allSnapshots = positionForExperiment(variablesDescriptor)
    os.write.over(os.pwd / os.RelPath(path) / s"$name-$variablesDescriptor", write(allSnapshots))
    positionForExperiment -= variablesDescriptor
  }

  override def setup(environment: Environment[T, P]): Unit =
    os.makeDir.all(os.pwd / os.RelPath(path))
}

object FireStationPosition {
  case class FireStationSnapshot(time: Long, position: Iterable[(Double, Double)])
  implicit def fireStationSnapshotRM: ReadWriter[FireStationSnapshot] = macroRW
}
