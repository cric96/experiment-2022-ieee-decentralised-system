package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.*

trait BlockSWithProcesses:
  self: AggregateProgram with CustomSpawn =>
  type SymmetryBreaker
  def localLeaderElection(
      id: ID,
      symmetryBreaker: SymmetryBreaker,
      radius: Double,
      metric: Metric,
      distance: Boolean => Double
  ): ID =
    val default = (id, (symmetryBreaker, 0))
    share(default)((local, nbrField) => local)
    mid()
