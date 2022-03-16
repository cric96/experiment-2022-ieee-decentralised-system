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
  lazy val grain = 3

  override def main(): Any = {
    val neighbours = excludingSelf.sumHood(nbr(1))
    node.put("water-level", perceiveWaterLevel())
    localLeaderElection(symmetryBreaker = neighbours, radius = grain)
    // localLeaderElection(mid(), neighbours, 1, nbrRange, source => distanceTo(source, nbrRange))
  }

  def perceiveWaterLevel(): Double =
    SensorTrace.perceive(currentPosition(), timestamp())
}
