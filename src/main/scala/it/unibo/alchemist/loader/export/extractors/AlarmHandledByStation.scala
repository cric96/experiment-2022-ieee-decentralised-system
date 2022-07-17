package it.unibo.alchemist.loader.`export`.extractors

import it.unibo.alchemist.loader.`export`.{ExportUtil, Extractor}
import it.unibo.alchemist.model.interfaces.{Environment, Reaction, Time}

import java.util

class AlarmHandledByStation extends Extractor[Int] {
  override def getColumnNames: util.List[String] = util.List.of("station-handle")
  override def extractData[T](
      environment: Environment[T, _],
      reaction: Reaction[T],
      time: Time,
      l: Long
  ): util.Map[String, Int] = {
    val fireStation = ExportUtil.initFireStations(environment)
    val ids = fireStation.map(_.get[Int]("solve-id")).filter(_ > 0).toSet
    util.Map.of("station-handle", ids.size)
  }
}
