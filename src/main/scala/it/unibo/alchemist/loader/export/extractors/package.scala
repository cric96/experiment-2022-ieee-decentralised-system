package it.unibo.alchemist.loader.`export`

import it.unibo.alchemist.model.implementations.nodes.{NodeManager, SimpleNodeManager}
import it.unibo.alchemist.model.interfaces.Environment

import scala.jdk.CollectionConverters.IteratorHasAsScala

package object extractors {

  private[extractors] def initFireStations[T](env: Environment[T, _]): List[SimpleNodeManager[T]] =
    env.getNodes
      .iterator()
      .asScala
      .toList
      .map(node => new SimpleNodeManager[T](node))
      .filter(_.has("fire"))
}
