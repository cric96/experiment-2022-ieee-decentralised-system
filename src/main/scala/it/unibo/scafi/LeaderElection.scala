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
  lazy val grain = node.get[Double]("grain")
  lazy val waterLevelWeight = node.get[Double]("waterLevelWeight")
  override def main(): Any = {
    val waterLevel = perceiveWaterLevel()
    val altitude = altitudeLevel()
    val altitudeMetric: Metric = () => math.hypot(nbrRange(), altitude - nbr(altitude))
    val waterLevelMetric: Metric = () =>
      (1 - waterLevelWeight) * nbrRange() + waterLevelWeight * (waterLevel - nbr(waterLevel))
    val waterArea =
      localLeaderElection(symmetryBreaker = mid(), radius = grain, distance = fastGradient(_, waterLevelMetric))
    val altitudeArea =
      localLeaderElection(symmetryBreaker = mid(), radius = grain, distance = fastGradient(_, altitudeMetric))
    node.put("water-level", waterLevel)
    node.put("altitude", altitude)
    waterArea
  }

  private def altitudeLevel(): Double = AltitudeService.in(currentPosition()._1, currentPosition()._2)
  private def perceiveWaterLevel(): Double =
    SensorTrace.perceive(currentPosition(), alchemistTimestamp.toDouble)

  private def fastGradient(source: Boolean, metric: Metric): Double = {
    share(Double.PositiveInfinity) { case (l, nbrg) =>
      mux(source)(0.0)(minHoodPlus(nbrg() + metric()))
    }
  }
}
