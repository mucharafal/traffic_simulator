package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.simulator.simulation.actor.Car.Crash
import com.simulator.simulation.actor.Junction._

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

  case object GetStatus
  final case class GetStatusResult(synchronizer: Int,
                                   junctionType: JunctionTypes,
                                   inRoads: List[ActorRef],
                                   outRoads: List[ActorRef],
                                   informationPackage: Any = ()) // TODO: don't use Any

  sealed trait Direction
  final object OutDirection extends Direction
  final object InDirection extends Direction

  final case class AddRoad(id: ActorRef, direction: Direction)
  final case class Turning(from: ActorRef, to: ActorRef)
}

abstract class Junction extends Actor {
  val log = Logging(context.system, this)

  var inRoads: List[ActorRef] = List.empty
  var outRoads: List[ActorRef] = List.empty

  var turnings: List[(ActorRef, ActorRef, ActorRef)] = List.empty  //who, from, to

  override def preStart() {
    log.info("Started")
  }

  def addRoad(roadId: ActorRef, direction: Direction) = {
    direction match {
      case InDirection =>
        inRoads :+= roadId
        log.info(s"Added in road ${roadId.path}")
      case OutDirection =>
        outRoads :+= roadId
        log.info(s"Added out road ${roadId.path}")
    }
  }
  def checkCollisionInTurn = {
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
    turnings = List.empty
  }
  def receive = {
    case Turning(from, to) =>
      turnings :+= (sender(), from, to)
    case AddRoad(id, map) =>
      addRoad(id, map)
  }
}

class RightHandJunction() extends Junction {

  import Junction._
  import JunctionTypes._
  import TimeSynchronizer._

  var synchronizer: Int = -1

  override def receive = super.receive orElse {
    case GetStatus =>
      sender() ! GetStatusResult(synchronizer, rightHandJunction, inRoads, outRoads)
    case ComputeTimeSlot(s) =>
      synchronizer = s
      super.checkCollisionInTurn
      sender() ! InfrastructureComputed
  }
}

object SignJunction {
  final case class PriviledgeRoad(road: ActorRef)
}
class SignJunction() extends Junction {

  import Junction._
  import JunctionTypes._
  import SignJunction._
  import TimeSynchronizer._

  var synchronizer: Int = -1
  var privilegedRoads: (ActorRef, ActorRef) = (null, null)

  override def receive = super.receive orElse {
    case GetStatus =>
      sender() ! GetStatusResult(synchronizer, signJunction, inRoads, outRoads, privilegedRoads)
    case ComputeTimeSlot(s) =>
      synchronizer = s
      super.checkCollisionInTurn
      sender() ! InfrastructureComputed
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

  import Junction._
  import JunctionTypes._
  import TimeSynchronizer._

  var greenLightRoadRef: ActorRef = null
  var timeToChange: Int = greenLightTime
  var synchronizer: Int = -1
  var roadIterator: Int = 0

  override def receive = super.receive orElse {
    case GetStatus =>
      sender() ! GetStatusResult(synchronizer, signalizationJunction, inRoads, outRoads, (greenLightRoadRef, timeToChange))
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
      super.checkCollisionInTurn
      sender() ! InfrastructureComputed
  }
}

