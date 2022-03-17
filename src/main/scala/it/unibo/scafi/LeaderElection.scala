package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.geo.altitude.AltitudeService
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
    val waterLevel = perceiveWaterLevel()
    val altitude = altitude()
    val election = localLeaderElection(symmetryBreaker = neighbours, radius = grain)
    node.put("water-level", waterLevel)
    node.put("altitude", altitude)
    election
  }

  def altitude(): Double = AltitudeService.in(currentPosition()._1, currentPosition()._2)
  def perceiveWaterLevel(): Double =
    SensorTrace.perceive(currentPosition(), alchemistTimestamp.toDouble)
}
