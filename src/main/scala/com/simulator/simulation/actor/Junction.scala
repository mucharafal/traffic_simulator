package com.simulator.simulation.actor

import akka.Done
import akka.actor.{Actor, Props}
import akka.event.Logging
import com.simulator.common.JunctionId
import com.simulator.simulation.actor.Junction.Direction

object Junction {

  def props(junctionId: JunctionId, greenLightInterval: Int): Props =
    Props(new Junction(junctionId, greenLightInterval))

  case object GetState
  final case class State(junctionId: JunctionId,
                         inRoads: List[RoadRef],
                         outRoads: List[RoadRef],
                         roadWithGreenLight: Option[RoadRef],
                         timeToChange: Double)

  sealed trait Direction
  object Direction {
    final object Out extends Direction
    final object In extends Direction
  }

  final case class AddRoad(id: RoadRef, direction: Direction)
}

class Junction(val junctionId: JunctionId,
               val greenLightInterval: Int) extends Actor {

  val log = Logging(context.system, this)

  var inRoads: List[RoadRef] = List.empty
  var outRoads: List[RoadRef] = List.empty

  var timeToChange: Int = greenLightInterval
  var greenLightRoad: Option[RoadRef] = None
  var greenLightRoadQueue: List[RoadRef] = List.empty

  override def preStart() {
    log.info("Started")
  }

  override def receive = {
    case Junction.GetState =>
      sender ! Junction.State(junctionId, inRoads, outRoads, greenLightRoad, timeToChange)

    case TimeSynchronizer.ComputeTimeSlot =>
      log.info("Computing time slot")

      if (timeToChange == 0) {
        timeToChange = greenLightInterval

        if (greenLightRoadQueue.isEmpty) {
          greenLightRoadQueue = inRoads
        }

        greenLightRoad = Some(greenLightRoadQueue.head)
        greenLightRoadQueue = greenLightRoadQueue.tail

        log.info(s"${ greenLightRoad.get.path } has green light")
      }

      timeToChange -= 1
      sender ! TimeSynchronizer.TimeSlotComputed

    case Junction.AddRoad(road, direction) =>
      direction match {
        case Direction.In =>
          inRoads ::= road
          log.info(s"Added in road ${ road.path }")
        case Direction.Out =>
          outRoads ::= road
          log.info(s"Added out road ${ road.path }")
      }
      sender ! Done
  }
}

