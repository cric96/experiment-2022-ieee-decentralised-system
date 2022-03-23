package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.geo.altitude.AltitudeService
import it.unibo.scafi.incarnation.{BlockSWithProcesses, Distance, ProcessFix}
class LeaderElection
    extends AggregateProgram
    with StandardSensors
    with ScafiAlchemistSupport
    with CustomSpawn
    with ProcessFix
    with BlockG
    with BlockSWithProcesses {
  private val rainGaugeTrace = SensorTrace
  private lazy val grain = node.get[Double]("grain")
  override def main(): Any = {
    val waterLevel = perceiveWaterLevel()
    val altitude = altitudeLevel()
    val altitudeMetric: Metric = () => math.hypot(nbrRange(), altitude - nbr(altitude))
    val waterLevelMetric: Metric = () => nbrRange() * (1 + math.abs(waterLevel - nbr(waterLevel)))
//      nbrRange() * (1 + waterLevel + nbr(waterLevel))
//      (1 - waterLevelWeight) * nbrRange() + waterLevelWeight * math.abs(waterLevel + nbr(waterLevel) * adjustLevel)
    val waterArea =
      localLeaderElection(
        symmetryBreaker = waterLevel,
        radius = grain,
        distanceFunction = Distance(waterLevelMetric, fastGradient)
      )
//    val altitudeArea =
//      localLeaderElection(symmetryBreaker = mid(), radius = grain, distance = fastGradient(_, altitudeMetric))
    node.put("water-level", waterLevel)
    node.put("altitude", altitude)
    waterArea
  }

  private def altitudeLevel(): Double = AltitudeService.in(currentPosition()._1, currentPosition()._2)
  private def perceiveWaterLevel(): Double =
    rainGaugeTrace.perceive(currentPosition(), alchemistTimestamp.toDouble)

  private def fastGradient(metric: Metric, source: Boolean): Double = {
    share(Double.PositiveInfinity) { case (l, nbrg) =>
      mux(source)(0.0)(minHoodPlus(nbrg() + metric()))
    }
  }
}
