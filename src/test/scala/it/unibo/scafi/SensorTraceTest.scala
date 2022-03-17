package it.unibo.scafi

import it.unibo.scafi.space.Point3D
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
class SensorTraceTest {
  val referenceTime = 11700
  val epsilon = 1
  val position = Point3D(43.64768, -79.47811, 0)
  val perceived = 0.361
  @Test
  def checkSpaceTimeIndex(): Unit = {}
  assertEquals(SensorTrace.perceive(position, referenceTime), perceived)

  @Test
  def checkSpaceIndex(): Unit =
    assertEquals(SensorTrace.perceive(position, referenceTime + epsilon), perceived)
}
