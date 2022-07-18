package it.unibo.alchemist.loader.`export`.extractors

import it.unibo.alchemist.loader.`export`.{ExportUtil, Extractor}
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.interfaces
import it.unibo.alchemist.model.interfaces.{Environment, Position, Reaction}

import java.util

class AverageDistanceFromAlarm[P <: Position[P]] extends Extractor[Double] {
  override def getColumnNames: util.List[String] = util.List.of("average-distance")

  override def extractData[T](
      environment: Environment[T, _],
      reaction: Reaction[T],
      time: interfaces.Time,
      l: Long
  ): util.Map[String, Double] = {
    val unsafeEnvironment = environment.asInstanceOf[Environment[T, P]]
    val fireStation = ExportUtil.getFireStationsFromEnvironment(environment)
    val stationAndAlarm = fireStation
      .map(node => (node, node.get[Int]("solve-id")))
      .filter { case (_, alarm) => alarm > 0 }
      .groupMapReduce(_._2)(node => Set(node))(_ ++ _)

    val averageDistanceFromAlarm = stationAndAlarm
      .map { case (id, stations) => (new SimpleNodeManager[T](unsafeEnvironment.getNodeByID(id)), stations) }
      .map { case (nodeAlarm, stations) => selectNearestFrom(unsafeEnvironment, nodeAlarm, stations.map(_._1)) }
    val extractedData = if (averageDistanceFromAlarm.isEmpty) { 0 }
    else { averageDistanceFromAlarm.sum / averageDistanceFromAlarm.size }
    util.Map.of("average-distance", extractedData)
  }

  private def selectNearestFrom[T, P <: Position[P]](
      environment: Environment[T, P],
      alarm: SimpleNodeManager[T],
      stations: Set[SimpleNodeManager[T]]
  ): Double =
    stations
      .map(node => environment.getPosition(node.node))
      .map(position => environment.getPosition(alarm.node).distanceTo(position))
      .min
}
