package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import Ordered._
import Builtins._ // for type classes

trait BlockSWithProcesses {
  self: AggregateProgram with ProcessFix with BlockG with ScafiAlchemistSupport with StandardSensors =>
  import SpawnInterface._ // for statuses
  // Conversions
  implicit def boundedToOrdering[A: Bounded]: Ordering[A] = (x: A, y: A) => implicitly[Bounded[A]].compare(x, y)
  def bounded[A: Bounded]: Bounded[A] = implicitly[Bounded[A]]
  // Types
  type Distance =
    Boolean => Double // or in general, it could be type Distance[D: Numeric] = Boolean => D, or another type context
  // Data
  /** Represent data produced by a single aggregate process
    * @param symmetryBreaker
    *   the symmetry breaker shared by the leader
    * @param distanceFromLeader
    *   the distance (using a certain metric)
    */
  case class LeaderProcessOutput[S](symmetryBreaker: S, distanceFromLeader: Double)

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
  case class LeaderProcessInput[S](
      localLeader: ID,
      localId: ID,
      breaker: S,
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
  def localLeaderElection[S: Bounded](
      id: ID = mid(),
      symmetryBreaker: S,
      radius: Double,
      distanceFunction: Distance = distanceTo(_, nbrRange)
  ): ID = {
    val default = id -> LeaderProcessOutput[S](symmetryBreaker, 0.0)
    rep(default) { case (leadId, leadOutput) =>
      val shouldStartProcess = id == leadId || (symmetryBreaker, id) > (leadOutput.symmetryBreaker, leadId)
      // compute the leaders using processes, in jointing point multiple leader could exists
      val leaders: Map[ID, LeaderProcessOutput[S]] = sspawn2[ID, LeaderProcessInput[S], LeaderProcessOutput[S]](
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

  private def processDefinition[S: Bounded]: ID => LeaderProcessInput[S] => POut[LeaderProcessOutput[S]] = id =>
    input => {
      val (status, gradient) = insideBubble(id)(input) // I check this zone is inside the bubble when id is the leader
      branch(status == Terminated || status == External) { // if I am external or the process is terminated, return a default field
        POut(LeaderProcessOutput(implicitly[Bounded[S]].bottom, Double.PositiveInfinity), status)
      } {
        POut(
          expandBubble(bounded[S])(gradient)(id)(input),
          Output
        ) // Otherwise, I continue to expand the bubble in this zone
      }
    }

  private def insideBubble[S]: ID => LeaderProcessInput[S] => (Status, Double) =
    processId => { case LeaderProcessInput(localLeader, uid, _, radius, distanceFunction) =>
      branch(processId == uid && uid != localLeader) {
        // started the process, but I am not the leader anymore, so I suppress that process
        (Terminated, Double.PositiveInfinity)
      } {
        val distanceFromLeader = distanceFunction(processId == uid) // distance from the leader
        val inBubble = distanceFromLeader <= radius // check if this zone is inside the bubble
        val anyNodeInBubble = includingSelf.anyHood(nbr(localLeader) == processId)
        (mux(inBubble && anyNodeInBubble)(Output)(External), distanceFromLeader)
      }
    }

  private def expandBubble[S: Bounded]: Double => ID => LeaderProcessInput[S] => LeaderProcessOutput[S] =
    gradient =>
      processId => { case LeaderProcessInput(_, uid, breaker, _, _) =>
        val source = processId == uid
        // broadcast the breaker of this leader in the entire zone where
        LeaderProcessOutput(broadcastAlong(source, gradient, breaker), gradient)
      }

  // select the leader using the symmetric breaker
  private def selectLeader[S: Bounded](
      leaders: Map[ID, LeaderProcessOutput[S]]
  ): Option[(ID, LeaderProcessOutput[S])] = {
    leaders.reduceOption[(ID, LeaderProcessOutput[S])] {
      case (leaderA @ (idA, LeaderProcessOutput(breakerA, _)), leaderB @ (idB, LeaderProcessOutput(breakerB, _))) =>
        if ((breakerA, idA) > (breakerB, idB)) {
          leaderA
        } else {
          leaderB
        }
    }
  }

  def broadcastAlong[D: Bounded](source: Boolean, g: Double, data: D): D = {
    share(data) { case (_, nbrField) =>
      mux(source)(data)(includingSelf.minHoodSelector[Double, D](nbr(g))(nbrField()))
    }
  }
}
