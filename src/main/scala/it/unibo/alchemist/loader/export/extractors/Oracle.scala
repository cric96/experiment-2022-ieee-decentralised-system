package it.unibo.alchemist.loader.`export`.extractors

import it.unibo.alchemist.loader.`export`.Extractor
import it.unibo.alchemist.model.implementations.nodes.{NodeManager, SimpleNodeManager}
import it.unibo.alchemist.model.interfaces.{Environment, Reaction, Time}

import java.util
import scala.jdk.CollectionConverters.IteratorHasAsScala

class Oracle extends Extractor[Int] {
  override def getColumnNames: util.List[String] = util.List.of("station-handle")
  override def extractData[T](
      environment: Environment[T, _],
      reaction: Reaction[T],
      time: Time,
      l: Long
  ): util.Map[String, Int] = {
    val fireStation = initFireStations(environment)
    val ids = fireStation.map(_.get[Int]("solve-id")).filter(_ > 0).toSet
    util.Map.of("station-handle", ids.size)
  }
  Ordered
  private def initFireStations[T](env: Environment[T, _]): List[NodeManager] =
    env.getNodes
      .iterator()
      .asScala
      .toList
      .map(node => new SimpleNodeManager[T](node))
      .filter(_.has("fire"))
}
