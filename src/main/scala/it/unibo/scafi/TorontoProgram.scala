package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.geo.altitude.AltitudeService
import it.unibo.scafi.incarnation.{BlockSWithProcesses, Distance, ProcessFix}

class TorontoProgram
    extends AggregateProgram
    with StandardSensors
    with ScafiAlchemistSupport
    with CustomSpawn
    with ProcessFix
    with BlockG
    with BlockC
    with BlocksWithGC
    with BlockSWithProcesses {
  import SpawnInterface._
  private val unit: Unit = {}
  private val maxExportDanger = 10
  private val rainGaugeTrace = SensorTrace
  private lazy val grain = node.get[Double]("grain")
  private lazy val dangerLevel = node.get[Double]("dangerThr")

  override def main(): Any = {
    val waterLevel = perceiveWaterLevel()
    val altitude = altitudeLevel()
    val altitudeMetric: Metric = () => math.hypot(nbrRange(), altitude - nbr(altitude))
    val waterLevelMetric: Metric = () => nbrRange() * (1 + math.abs(waterLevel - nbr(waterLevel)))
    val waterArea =
      localLeaderElection(
        symmetryBreaker = waterLevel,
        radius = grain,
        distanceFunction = Distance(waterLevelMetric, fastGradient)
      )
    val altitudeArea =
      localLeaderElection(
        symmetryBreaker = altitude,
        radius = grain,
        distanceFunction = Distance(altitudeMetric, fastGradient)
      )
    val isInDanger = dangerSignal(waterArea, waterLevel)
    val actionNeeded = propagateDangerZone(altitudeArea, isInDanger.exists(_._2))
    val busy =
      mux(isFireStation && actionNeeded.nonEmpty)(1)(0)
    val stationChoice = rep(Option.empty[(ID, Double)]) {
      case some @ Some(elem) if (actionNeeded.exists(_._1 == elem._1)) => some
      case _ => actionNeeded.minByOption(_._2)
    }
    node.put("danger-map", isInDanger)
    node.put("id", mid())
    node.put("action-needed", actionNeeded)
    node.put("water-level", waterLevel)
    node.put("altitude", altitude)
    node.put("danger", isInDanger.exists(_._2))
    node.put("altitude-area", altitudeArea)
    node.put("water-level-area", waterArea)
    node.put("station-received", mux(isFireStation)(isInDanger.size)(0))
    node.put("station-busy", busy)
    node.put("total-danger", mux(actionNeeded.keySet.contains(mid()))(1)(0))
    node.put("solve", mux(isFireStation)(stationChoice.map(_._2).getOrElse(Double.NaN))(Double.NaN))
    node.put("solve-id", mux(isFireStation)(stationChoice.map(_._1).getOrElse(-1))(-1))
    dangerExport(actionNeeded)
    waterArea
  }

  private def isFireStation: Boolean = node.has("fire")
  private def altitudeLevel(): Double = AltitudeService.in(currentPosition()._1, currentPosition()._2)
  private def perceiveWaterLevel(): Double =
    rainGaugeTrace.perceive(currentPosition(), alchemistTimestamp.toDouble)

  private def fastGradient(metric: Metric, source: Boolean): Double = {
    share(Double.PositiveInfinity) { case (l, nbrg) =>
      mux(source)(0.0)(minHoodPlus(nbrg() + metric()))
    }
  }

  private def dangerSignal(waterArea: ID, waterLevel: Double): Map[ID, Boolean] = sspawn2[ID, Unit, Boolean](
    leader =>
      _ => {
        val leaderZone = leader == waterArea
        val source = leader == mid()
        val status = mux(!leaderZone) {
          // I am not in this area
          mux(source) {
            // I was the leader but I changed the area, this process is finished
            Terminated
          } {
            // I am external to this area
            External
          }
        }(Output)
        val potential = fastGradient(nbrRange, source)
        val count = C[Double, Int](potential, _ + _, 1, 0)
        val waterLevelArea = C[Double, Double](potential, _ + _, waterLevel, 0.0)
        val averageWaterLevel = waterLevelArea / count
        node.put("water-level-average", broadcast(source, averageWaterLevel))
        node.put("potential-danger", potential)
        val isInDanger = source && averageWaterLevel > dangerLevel && count > 1
        POut(
          broadcast(source, isInDanger),
          status
        )
      },
    mux(mid() == waterArea)(Set(waterArea))(Set.empty),
    unit
  )

  private def propagateDangerZone(altitudeZone: ID, danger: Boolean): Map[ID, Double] =
    sspawn2[ID, Unit, Double](
      leader =>
        _ => {
          val source = leader == mid()
          val distance = fastGradient(nbrRange, source)
          val status = mux(source && !danger)(Terminated)(mux(distance < grain)(Output)(External))
          POut(distance, status)
        },
      mux(altitudeZone == mid() && danger)(Set(mid()))(Set.empty[ID]),
      unit
    )

  private def dangerExport(dangers: Map[ID, Double]): Unit = {
    1 to maxExportDanger foreach (x => node.put(s"danger-$x", 0)) // clear
    1 to dangers.size foreach (x => node.put(s"danger-$x", 1)) // add dangers
  }

}