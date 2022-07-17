package it.unibo.alchemist.loader.`export`

import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.interfaces.Environment

import scala.jdk.CollectionConverters.IteratorHasAsScala

object ExportUtil {
  def initFireStations[T](env: Environment[T, _]): List[SimpleNodeManager[T]] =
    env.getNodes
      .iterator()
      .asScala
      .toList
      .map(node => new SimpleNodeManager[T](node))
      .filter(_.has("fire"))
}
