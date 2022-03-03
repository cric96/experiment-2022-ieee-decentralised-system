package it.unibo.scafi.incarnation
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

/** fix for export discard missing */
trait ProcessFix extends CustomSpawn {
  self: AggregateProgram =>
  // TODO: put to ScaFi framework
  override def runOnSharedKeysWithShare[K, A, R](process: K => (R, Boolean), params: Set[K]): Map[K, R] = {
    share(Map[K, R]())((_, nbr) => {
      (includingSelf
        .unionHoodSet(nbr().keySet ++ params))
        .mapToValues(k => exportConditionally(process.apply(k)))
        .collectValues[R] { case (r, true) => r }
    })
  }
}
