package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.{BlockSWithProcesses, ProcessFix}
import it.unibo.scafi.space.Point3D
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
    localLeaderElection(symmetryBreaker = neighbours, radius = grain)
    node.put("water-level", perceiveWaterLevel())
    node.put("id", mid())
  }

  def perceiveWaterLevel(): Double =
    SensorTrace.perceive(currentPosition(), alchemistTimestamp.toDouble)
}
