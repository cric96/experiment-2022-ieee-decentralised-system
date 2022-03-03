package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Ordered._ // help implicit conversion

trait BlockSWithProcesses {
  self: AggregateProgram with ProcessFix with BlockG with ScafiAlchemistSupport =>
  import SpawnInterface._ // for statuses
  type SymmetryBreaker = Double // but could be a lambda???
  type Distance =
    Boolean => Double // or in general, it could be type Distance[D: Numeric] = Boolean => D, or another type context

  case class LeaderProcessData(symmetryBreaker: SymmetryBreaker, distanceFromLeader: Double)
  case class LeaderProcessInput(
      localLeader: ID,
      localId: ID,
      symmetryBreaker: SymmetryBreaker,
      radius: Double,
      metric: Metric, // probably useless
      distance: Distance
  )

  def localLeaderElection(
      id: ID,
      symmetryBreaker: SymmetryBreaker,
      radius: Double,
      metric: Metric,
      distance: Distance
  ): ID = {
    val default = id -> LeaderProcessData(symmetryBreaker, 0.0)
    rep(default) { case (leadId, leadData) =>
      val leaders = sspawn2[ID, LeaderProcessInput, LeaderProcessData](
        processDefinition,
        Set(leadId),
        LeaderProcessInput(leadId, id, leadData.symmetryBreaker, radius, metric, distance)
      )
      node.put("howManyProcess", leaders.size)
      val closeEnough = leaders.filter { case (_, LeaderProcessData(_, distance)) => distance < radius }
      val best = selectLeader(closeEnough).getOrElse(default)
      best
    }._1
  }

  def localLeaderElection2(
      id: ID,
      symmetryBreaker: SymmetryBreaker,
      radius: Double,
      metric: Metric,
      distance: Distance
  ): ID = {
    val default = id -> LeaderProcessData(symmetryBreaker, 0.0)
    share(default) { case ((leadId, leadData), nbrField) =>
      val sources = includingSelf.unionHood(nbrField())
      node.put("sources", sources.map(_._1))
      val processId: Set[ID] = sources.map { case (id, _) => id }
      val leaders = sspawn2[ID, LeaderProcessInput, LeaderProcessData](
        processDefinition,
        processId,
        LeaderProcessInput(leadId, id, leadData.symmetryBreaker, radius, metric, distance)
      )
      node.put("howManyProcess", leaders.size)
      val closeEnough = leaders.filter { case (_, LeaderProcessData(_, distance)) => distance < radius }
      val best = selectLeader(closeEnough).getOrElse(default)
      best
    }._1
  }

  private val processDefinition: ID => LeaderProcessInput => POut[LeaderProcessData] = id =>
    breaker => {
      val status = insideBubble(id)(breaker)
      mux(status == Terminated || status == External) {
        POut(LeaderProcessData(0, Double.PositiveInfinity), status)
      } {
        POut(expandBubble(id)(breaker), Output)
      }
    }

  private val insideBubble: ID => LeaderProcessInput => Status =
    processId => { case LeaderProcessInput(localLeader, uid, _, radius, _, distance) =>
      branch(processId == uid && uid != localLeader) {
        Terminated // I started the process, but I am not the leader anymore, so I suppress that process
      } {
        val inBubble = distance(processId == uid) <= radius // probably it does not work
        mux(inBubble)(Output)(External)
      }
    }

  private val expandBubble: ID => LeaderProcessInput => LeaderProcessData =
    processId => { case LeaderProcessInput(localLeader, localId, symmetryBreaker, _, _, distance) =>
      val source = processId == localId && localLeader == localId
      val gradient = distance(source)
      LeaderProcessData(broadcastAlong(source, gradient, symmetryBreaker), gradient)
    }

  private def selectLeader(leaders: Map[ID, LeaderProcessData]): Option[(ID, LeaderProcessData)] = {
    leaders.reduceOption[(ID, LeaderProcessData)] {
      case (leaderA @ (idA, LeaderProcessData(breakerA, _)), leaderB @ (idB, LeaderProcessData(breakerB, _))) =>
        if ((breakerA, idA) > (breakerB, idB)) {
          leaderA
        } else {
          leaderB
        }
    }
  }

  def broadcastAlong[D: Builtins.Bounded](source: Boolean, g: Double, data: D): D = {
    share(data) { case (local, nbrField) =>
      mux(source)(local)(includingSelf.minHoodSelector[Double, D](nbr(g))(nbr(nbrField())))
    }
  }

}
