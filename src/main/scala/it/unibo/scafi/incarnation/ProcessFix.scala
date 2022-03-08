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

  override def sspawn2[K, A, R](process: K => A => POut[R], params: Set[K], args: A): Map[K, R] =
    spawn2[K, A, Option[R]](process.map(handleTermination).map(handleOutput), params, args)
      .collectValues { case Some(p) => p }
}
