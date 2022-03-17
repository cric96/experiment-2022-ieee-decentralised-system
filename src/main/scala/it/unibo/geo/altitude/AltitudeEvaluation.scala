package it.unibo.geo.altitude

trait AltitudeEvaluation {
  def in(lat: Double, long: Double): Double
}
