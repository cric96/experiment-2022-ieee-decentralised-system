package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.Metric

case class Distance(metric: Metric, logic: (Metric, Boolean) => Double) {
  def apply(source: Boolean): Double = logic(metric, source)
}
