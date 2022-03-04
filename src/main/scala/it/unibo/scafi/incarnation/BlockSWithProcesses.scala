package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Ordered._ // help implicit conversion

/*
TODO:
- use metric to expand and distance to broadcast the information
- maintains the bubble active even if the node is not a leader
- pass a function in which is possible to "drop" the bubble

 */
trait BlockSWithProcesses {
  self: AggregateProgram with ProcessFix with BlockG with ScafiAlchemistSupport with StandardSensors =>
  import SpawnInterface._ // for statuses
  type SymmetryBreaker = Double // but could be a lambda???
  type Distance =
    Boolean => Double // or in general, it could be type Distance[D: Numeric] = Boolean => D, or another type context

  case class LeaderProcessOutput(symmetryBreaker: SymmetryBreaker, distanceFromLeader: Double)
  case class LeaderProcessInput(
      localLeader: ID,
      localId: ID,
      data: LeaderProcessOutput,
      radius: Double,
      metric: Metric, // probably useless
      distance: Distance
  )

  def localLeaderElection(
      id: ID = mid(),
      symmetryBreaker: SymmetryBreaker,
      radius: Double,
      metric: Metric = nbrRange,
      distance: Distance = distanceTo(_, nbrRange)
  ): ID = {
    val default = id -> LeaderProcessOutput(symmetryBreaker, 0.0)
    rep(default) { case (leadId, leadData) =>
      val leaders = sspawn2[ID, LeaderProcessInput, LeaderProcessOutput](
        processDefinition,
        Set(leadId),
        LeaderProcessInput(leadId, id, leadData, radius, metric, distance)
      )
      val closeEnough = leaders.filter { case (_, LeaderProcessOutput(_, distance)) => distance < radius }
      val best = selectLeader(closeEnough).getOrElse(default)
      best
    }._1
  }

  private val processDefinition: ID => LeaderProcessInput => POut[LeaderProcessOutput] = id =>
    input => {
      val (status, gradient) = insideBubble(id)(input)
      branch(status == Terminated || status == External) {
        POut(LeaderProcessOutput(Double.NegativeInfinity, Double.PositiveInfinity), status)
      } {
        POut(expandBubble(gradient)(id)(input), Output)
      }
    }

  private val insideBubble: ID => LeaderProcessInput => (Status, Double) =
    processId => { case LeaderProcessInput(localLeader, uid, _, radius, _, distance) =>
      branch(processId == uid && uid != localLeader) {
        // started the process, but I am not the leader anymore, so I suppress that process
        (Terminated, Double.PositiveInfinity)
      } {
        val distanceFromLeader = distance(processId == uid)
        val inBubble = distanceFromLeader <= radius // probably it does not work
        (mux(inBubble)(Output)(External), distanceFromLeader)
      }
    }

  private val expandBubble: Double => ID => LeaderProcessInput => LeaderProcessOutput =
    gradient =>
      processId => { case LeaderProcessInput(localLeader, uid, leadData, _, _, _) =>
        val source = processId == uid
        LeaderProcessOutput(broadcastAlong(source, gradient, leadData.symmetryBreaker), gradient)
      }

  private def selectLeader(leaders: Map[ID, LeaderProcessOutput]): Option[(ID, LeaderProcessOutput)] = {
    leaders.reduceOption[(ID, LeaderProcessOutput)] {
      case (leaderA @ (idA, LeaderProcessOutput(breakerA, _)), leaderB @ (idB, LeaderProcessOutput(breakerB, _))) =>
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
