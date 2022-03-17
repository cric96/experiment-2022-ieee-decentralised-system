package it.unibo.geo.altitude
class CanadianService extends AltitudeEvaluation {
  private val endpoint = "http://geogratis.gc.ca/services/elevation/cdem/altitude"
  // altitude?lat=45.5&lon=-71.5
  override def in(lat: Double, lon: Double): Double = {
    val request = requests.get(endpoint, params = Map("lat" -> lat.toString, "lon" -> lon.toString))
    val data = ujson.read(request)
    data("altitude").num
  }

  private def completeQuery(lat: Double, lon: Double): String = s"$endpoint?lat=$lat&long=$lon"
}
