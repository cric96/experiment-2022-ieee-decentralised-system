package it.unibo.geo.altitude

trait AltitudeEvaluation {
  def in(lat: Double, lon: Double): Double
}
