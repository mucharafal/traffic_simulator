package com.simulator

import akka.actor.{Actor, ActorRef, Props}
import com.simulator.Junction.{Direction, InDirection, OutDirection, Turning}

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

  case object JunctionGetInformationRequest
  final case class JunctionGetInformationResult(synchronizer: Int, informationPackage: Any) // TODO: don't use Any

  sealed trait Direction
  final object OutDirection extends Direction
  final object InDirection extends Direction

  final case class AddRoad(id: ActorRef, direction: Direction)
  final case class Turning(from: ActorRef, to: ActorRef)
}

abstract class Junction extends Actor {
  import Junction._
  import Car._
  var inRoads: List[ActorRef] = List.empty
  var outRoads: List[ActorRef] = List.empty

  var turnings: List[(ActorRef, ActorRef, ActorRef)] = List.empty  //who, from, to

  def addRoad(roadId: ActorRef, direction: Direction) = {
    direction match {
      case InDirection => inRoads :+= roadId
      case OutDirection => outRoads :+= roadId
    }
  }
  def checkCollision = {
    var source = Actor.noSender
    var wasCollision = false
    for((_,from, _) <- turnings){
      val From = from
      source match {
        case Actor.noSender =>
          source = from
        case From =>
        case _ => wasCollision = true
      }
    }
    if(wasCollision)  turnings.foreach({case (car, _, _) => car ! Crash})
  }
  def receive = {
    case Turning(from, to) =>
      turnings :+= (sender(), from, to)
    case AddRoad(id, map) =>
      addRoad(id, map)
  }
}

class RightHandJunction() extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._

  var synchronizer: Int = -1

  override def receive = super.receive orElse {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, (rightHandJunction, inRoads, outRoads))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      super.checkCollision
      sender() ! InfrastructureComputed
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
  var privilegedRoads: (ActorRef, ActorRef) = (Actor.noSender, Actor.noSender)

  override def receive = super.receive orElse {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, (signJunction, inRoads, outRoads, privilegedRoads))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! InfrastructureComputed
    case PriviledgeRoad(ref) =>
      privilegedRoads = privilegedRoads match {
        case (Actor.noSender, Actor.noSender) =>
          (ref, Actor.noSender)
        case (sth, Actor.noSender) =>
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

  override def receive = super.receive orElse {
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
      sender() ! InfrastructureComputed
  }
}

