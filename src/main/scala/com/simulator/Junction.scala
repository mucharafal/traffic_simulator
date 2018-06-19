package com.simulator

import akka.actor.{Props, ActorRef, Actor}

object JunctionTypes extends Enumeration {
  type JunctionTypes = Value
  val rightHandJunction, signJunction, signalizationJunction = Value
}

object Junction {
  import JunctionTypes._
  def props(junctionType: JunctionTypes): Props = {
    junctionType match {
      case JunctionTypes.rightHandJunction =>
        Props(new RightHandJunction())
      case JunctionTypes.signJunction =>
        Props(new SignJunction())
      case JunctionTypes.signalizationJunction =>
        Props(new SignalizationJunction())
    }
  }
  final case class JunctionGetInformationResult(synchronizer: Int, informationPackage: Any) // TODO: don't use Any
  final case class AddRoad(id: ActorRef, roadInformation: Map[String, Any]) // TODO: don't use Any
  case object JunctionGetInformationRequest
}

abstract class Junction extends Actor {
  var inRoads: List[ActorRef] = List.empty
  var outRoads: List[ActorRef] = List.empty

  def addRoad(roadId: ActorRef, roadInformation: Map[String, Any]) = { // TODO: don't use Any
    val begin: Boolean = roadInformation("begin").asInstanceOf[Boolean]
    if (begin) {
      outRoads :+= roadId
    } else {
      inRoads :+= roadId
    }
  }
}

class RightHandJunction() extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._

  var synchronizer: Int = -1

  def receive = {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, (rightHandJunction, inRoads, outRoads))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! Computed
    case AddRoad(id, map) =>
      super.addRoad(id, map)
  }
}

object SignJunction {
  final case class PriviledgeRoad(road: ActorRef)
}
class SignJunction() extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._
  import SignJunction._

  var synchronizer: Int = -1
  var privilegedRoads: (ActorRef, ActorRef) = (null, null)

  def receive = {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, (signJunction, inRoads, outRoads, privilegedRoads))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! Computed
    case AddRoad(id, map) =>
      super.addRoad(id, map)
    case PriviledgeRoad(ref) =>
      privilegedRoads = privilegedRoads match {
        case (null, null) =>
          (ref, null)
        case (sth, null) =>
          (sth, ref)
        case (sth, sth2) =>
          privilegedRoads
      }
  }
}

class SignalizationJunction(val greenLightTime: Int = 10) extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._

  var greenLightRoadRef: ActorRef = null
  var timeToChange: Int = greenLightTime
  var synchronizer: Int = -1
  var roadIterator: Int = 0

  def receive = {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, (signalizationJunction, inRoads, outRoads, greenLightRoadRef, timeToChange))
    case ComputeTimeSlot(s) =>
      timeToChange match {
        case 0 =>
          timeToChange = greenLightTime
          if (inRoads.nonEmpty) {
            greenLightRoadRef = inRoads(roadIterator)
            roadIterator = (roadIterator + 1) % inRoads.size
          }
        case _ =>
          timeToChange -= 1
      }
      synchronizer = s
      sender() ! Computed
    case AddRoad(id, map) =>
      super.addRoad(id, map)
  }
}
