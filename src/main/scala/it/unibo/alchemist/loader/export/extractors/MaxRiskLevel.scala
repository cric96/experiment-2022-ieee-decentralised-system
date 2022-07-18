package it.unibo.alchemist.loader.`export`.extractors

import it.unibo.alchemist.loader.`export`.Extractor
import it.unibo.alchemist.model.interfaces
import it.unibo.alchemist.model.interfaces.{Environment, Position, Reaction}
import it.unibo.geo.altitude.RiskService

import java.util

class MaxRiskLevel[P <: Position[P]] extends Extractor[Double] {
  override def getColumnNames: util.List[String] = util.List.of("max-risk")

  override def extractData[T](
      environment: Environment[T, _],
      reaction: Reaction[T],
      time: interfaces.Time,
      l: Long
  ): util.Map[String, Double] =
    util.Map.of("max-risk", RiskService.riskMapAt(time).maxBy(_.risk).risk)
}
