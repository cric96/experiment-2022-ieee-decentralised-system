package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Ordered._ // help implicit conversion

trait BlockSWithProcesses {
  self: AggregateProgram with ProcessFix with BlockG with ScafiAlchemistSupport with StandardSensors =>
  import SpawnInterface._ // for statuses
  // Types
  type SymmetryBreaker = Double // but could be a lambda???
  type Distance =
    Boolean => Double // or in general, it could be type Distance[D: Numeric] = Boolean => D, or another type context
  // Data
  /** Represent data produced by a single aggregate process
    * @param symmetryBreaker
    *   the symmetry breaker shared by the leader
    * @param distanceFromLeader
    *   the distance (using a certain metric)
    */
  case class LeaderProcessOutput(symmetryBreaker: SymmetryBreaker, distanceFromLeader: Double)

  /** @param localLeader
    *   the current leader field produce by processes
    * @param localId
    *   local id fields
    * @param breaker
    *   simmetry breaker used during the leader election
    * @param radius
    *   the maxiumum influence area of a leader
    * @param distance
    *   the strategy used to compute the distance from the leader
    */
  case class LeaderProcessInput(
      localLeader: ID,
      localId: ID,
      breaker: SymmetryBreaker,
      radius: Double,
      distance: Distance
  )

  /** This function produces a field of id resulted from a distributed leader election process.
    * @param id
    *   the id node fields
    * @param symmetryBreaker
    *   simmetry breaker used during the leader election
    * @param radius
    *   the maxiumum influence area of a leader
    * @param distanceFunction
    *   the strategy used to compute the distance from the leader
    * @return
    */
  def localLeaderElection(
      id: ID = mid(),
      symmetryBreaker: SymmetryBreaker,
      radius: Double,
      distanceFunction: Distance = distanceTo(_, nbrRange)
  ): ID = {
    val default = id -> LeaderProcessOutput(symmetryBreaker, 0.0)
    rep(default) { case (leadId, leadOutput) =>
      val shouldStartProcess = id == leadId || (symmetryBreaker, id) > (leadOutput.symmetryBreaker, leadId)
      // compute the leaders using processes, in jointing point multiple leader could exists
      val leaders: Map[ID, LeaderProcessOutput] = sspawn2[ID, LeaderProcessInput, LeaderProcessOutput](
        processDefinition,
        mux(shouldStartProcess)(Set(id))(Set.empty), // a process is spawn only if I am the local candidate
        LeaderProcessInput(leadId, id, symmetryBreaker, radius, distanceFunction)
      )
      node.put("leaders", leaders)
      val closeEnough = leaders.filter { case (_, LeaderProcessOutput(_, distance)) => distance < radius }
      node.put("close-enough", closeEnough)
      // choose the leader using the breaking symmetry value
      selectLeader(closeEnough).getOrElse(default)
    }._1
  }

  private val processDefinition: ID => LeaderProcessInput => POut[LeaderProcessOutput] = id =>
    input => {
      val (status, gradient) = insideBubble(id)(input) // I check this zone is inside the bubble when id is the leader
      branch(status == Terminated || status == External) { // if I am external or the process is terminated, return a default field
        POut(LeaderProcessOutput(Double.NegativeInfinity, Double.PositiveInfinity), status)
      } {
        POut(expandBubble(gradient)(id)(input), Output) // Otherwise, I continue to expand the bubble in this zone
      }
    }

  private val insideBubble: ID => LeaderProcessInput => (Status, Double) =
    processId => { case LeaderProcessInput(localLeader, uid, _, radius, distanceFunction) =>
      branch(processId == uid && uid != localLeader) {
        // started the process, but I am not the leader anymore, so I suppress that process
        (Terminated, Double.PositiveInfinity)
      } {
        val distanceFromLeader = distanceFunction(processId == uid) // distance from the leader
        val inBubble = distanceFromLeader <= radius // check if this zone is inside the bubble
        (mux(inBubble)(Output)(External), distanceFromLeader)
      }
    }

  private val expandBubble: Double => ID => LeaderProcessInput => LeaderProcessOutput =
    gradient =>
      processId => { case LeaderProcessInput(_, uid, breaker, _, _) =>
        val source = processId == uid
        // broadcast the breaker of this leader in the entire zone where
        LeaderProcessOutput(broadcastAlong(source, gradient, breaker), gradient)
      }

  // select the leader using the symmetric breaker
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
      mux(source)(data)(includingSelf.minHoodSelector[Double, D](nbr(g))(nbrField()))
    }
  }
}
