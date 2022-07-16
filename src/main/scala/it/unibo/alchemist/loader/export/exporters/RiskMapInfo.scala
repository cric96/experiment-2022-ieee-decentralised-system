package it.unibo.alchemist.loader.`export`.exporters

import upickle.default.macroRW

object RiskMapInfo {
  case class RiskSnapshot(time: Long, level: Iterable[RiskLevel])
  case class RiskLevel(lat: Double, lon: Double, risk: Double)
  implicit def macroRiskLevel: upickle.default.ReadWriter[RiskLevel] = macroRW
  implicit def macroRiskSnapshot: upickle.default.ReadWriter[RiskSnapshot] = macroRW
}
