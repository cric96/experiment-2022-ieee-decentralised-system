package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.{BlockSWithProcesses, ProcessFix}
class LeaderElection
    extends AggregateProgram
    with StandardSensors
    with ScafiAlchemistSupport
    with CustomSpawn
    with ProcessFix
    with BlockG
    with BlockSWithProcesses {
  override def main(): Any = {
    val neighbours = excludingSelf.sumHood(nbr(1))
    localLeaderElection(symmetryBreaker = neighbours, radius = 3)
    // localLeaderElection(mid(), neighbours, 1, nbrRange, source => distanceTo(source, nbrRange))
  }
}
