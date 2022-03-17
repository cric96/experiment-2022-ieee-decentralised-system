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
  lazy val grain = 20000

  override def main(): Any = {
    val waterLevel = perceiveWaterLevel()
    val altitude = altitudeLevel()
    val waterArea =
      localLeaderElection(symmetryBreaker = mid(), radius = grain, distance = distanceTo(_, () => waterLevel))
    val altitudeArea =
      localLeaderElection(symmetryBreaker = mid(), radius = grain, distance = distanceTo(_, () => altitude))
    node.put("water-level", waterLevel)
    node.put("altitude", altitude)
    waterArea
  }

  def altitudeLevel(): Double = AltitudeService.in(currentPosition()._1, currentPosition()._2)
  def perceiveWaterLevel(): Double =
    SensorTrace.perceive(currentPosition(), alchemistTimestamp.toDouble)
}
