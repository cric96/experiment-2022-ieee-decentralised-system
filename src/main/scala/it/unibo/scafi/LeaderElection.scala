package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.geo.altitude.AltitudeService
import it.unibo.scafi.incarnation.{BlockSWithProcesses, Distance, ProcessFix}

/** Idea: in un area c'era un possibile allerme. Vedo se c'è un punto critico (leader che è sotto una certa solgia)
  * Questo, se riceve l'allarme, fa partire un processo che espande l'allerme ocn distanza e id di chi l'ha generato. Le
  * stazioni poi, scelgono un id a cui associarsi La somma totale di allarmi deve tendere al numero totale di leader
  */
class LeaderElection
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
  private val dangerLevel = 0.8
  private val maxExportDanger = 10
  private val rainGaugeTrace = SensorTrace
  private lazy val grain = node.get[Double]("grain")
  // avverte tutti dentro 3 km di raggio
  // fire station => bisogna allargarsi
  // lanci un processo dal leader che trova la più vicina fire station
  // trovi le tre centrali più vicine ==> trovate quelle più vicine (grain infinito), quelle più vicine.
  // se sono il più vicino in assoluto alla centrale allerto solo quella.
  // devo vedere gli altri che centrali ha trovato
  // più di uno troverà la stessa => minimizzo la distanza media
  // stazione mi espando al più vicino leader. propago l'allerme con la distanza e l'id del leader. se hai distanza identiche => id
  // uno e una soltanto ==>
  // conto i warning ricevuti
  // exportare 10 variabile
  // 10 warning (conto 1 se ho almeno n warning)
  // distanza da chi processa il warning
  // SE non le allochiamo uno e una soltano, contiamo
  // numero di allarmi ricevuto per ogni stazione
  // numero di massimo di cose che sto seguendo per stazione,
  // piovtisà
  // numero di allarmi totali
  // numero di stazioni allertate
  // totale delle stazioni che sta agendo
  // numero di allert che ha ricevuto
  // la media di allert ha ricevuto
  // 33 simulazioni su iris
  // seed che vanno da 33 a 99
  // se hai più alert, dai più valore all'altezza
  // in base all'altitudine, propagi il vero allert
  //
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
    val busy = mux(isFireStation && actionNeeded.size > 1)(1)(0)
    node.put("danger-map", isInDanger)
    node.put("action-needed", actionNeeded)
    node.put("water-level", waterLevel)
    node.put("altitude", altitude)
    node.put("danger", isInDanger.exists(_._2))
    node.put("altitude-area", altitudeArea)
    node.put("water-level-area", waterArea)
    node.put("station-received", mux(isFireStation)(isInDanger.size)(0))
    node.put("station-busy", busy)
    node.put("solve", mux(isFireStation)(actionNeeded.minByOption(_._2).map(_._1).getOrElse(0))(0))
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
        val status = mux(!leaderZone) {
          // I am not in this area
          mux(leader == mid) {
            // I was the leader but I changed the area, this process is finished
            Terminated
          } {
            // I am external to this areaempty
            External
          }
        }(Output)
        val potential = fastGradient(nbrRange, mid == leader)
        val count = C[Double, Int](potential, _ + _, 1, 0)
        val waterLevelArea = C[Double, Double](potential, _ + _, waterLevel, 0.0)
        node.put("water-level-average", waterLevelArea)
        POut(leader == mid && waterLevelArea / count > dangerLevel && count > 1, status)
      },
    Set(waterArea),
    waterLevel
  )

  private def propagateDangerZone(altitudeZone: ID, danger: Boolean): Map[ID, Double] =
    sspawn2[ID, Unit, Double](
      leader =>
        _ => {
          val status = mux(altitudeZone == leader && !danger) {
            Terminated
          }(Output)
          POut(fastGradient(nbrRange, altitudeZone == leader), status)
        },
      mux(altitudeZone == mid() && danger)(Set(mid()))(Set.empty[ID]),
      ()
    )

  private def dangerExport(dangers: Map[ID, Double]): Unit = {
    1 to maxExportDanger foreach (x => node.put(s"danger-$x", 0)) // clear
    1 to dangers.size foreach (x => node.put(s"danger-$x", 1)) // add dangers
  }

}
