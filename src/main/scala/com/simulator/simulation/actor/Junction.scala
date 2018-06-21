package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.simulator.simulation.actor.Junction.{InDirection, OutDirection}

object JunctionTypes extends Enumeration {
  type JunctionTypes = Value
  val rightHandJunction, signJunction, signalizationJunction = Value
}

object Junction {

  def props(): Props = Props(new Junction())

  case object GetStatus
  final case class GetStatusResult(synchronizer: Int,
                                   inRoads: List[ActorRef],
                                   outRoads: List[ActorRef],
                                   informationPackage: Any = ()) // TODO: don't use Any

  sealed trait Direction
  final object OutDirection extends Direction
  final object InDirection extends Direction

  final case class AddRoad(id: ActorRef, direction: Direction)
}


class Junction(val greenLightTime: Int = 10) extends Actor {

  val log = Logging(context.system, this)

  var inRoads: List[ActorRef] = List.empty
  var outRoads: List[ActorRef] = List.empty

  var greenLightRoadRef: ActorRef = null
  var timeToChange: Int = greenLightTime
  var synchronizer: Int = -1
  var roadIterator: Int = 0

  override def preStart() {
    log.info("Started")
  }

  override def receive = {
    case Junction.GetStatus =>
      sender() ! Junction.GetStatusResult(synchronizer, inRoads, outRoads, (greenLightRoadRef, timeToChange))
    case TimeSynchronizer.ComputeTimeSlot(s) =>
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
      sender() ! TimeSynchronizer.InfrastructureComputed
    case Junction.AddRoad(roadRef, direction) =>
      direction match {
        case InDirection =>
          inRoads :+= roadRef
          log.info(s"Added in road ${ roadRef.path }")
        case OutDirection =>
          outRoads :+= roadRef
          log.info(s"Added out road ${ roadRef.path }")
      }
  }
}

