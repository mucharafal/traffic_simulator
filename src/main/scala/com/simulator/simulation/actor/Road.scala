package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.simulator.common.RoadId
import com.simulator.simulation.actor.TimeSynchronizer.ComputeTimeSlot

object Road {
  def props(roadId: RoadId, startJunction: JunctionRef, endJunction: JunctionRef, length: Double): Props =
    Props(new Road(roadId, startJunction, endJunction, length))

  case class EnterRoad(position: Double) // send by Car
  case object LeaveRoad // send by Car
}

class Road(val roadId: RoadId,
           val startJunction: JunctionRef,
           val endJunction: JunctionRef,
           val length: Double) extends Actor {

  import Road._

  val log = Logging(context.system, this)

  var cars = Seq.empty[(ActorRef, Double)]

  override def preStart() {
    log.info("Started")
  }

  override def receive = {
    case EnterRoad(position) =>
      val car = sender
      val carAhead = cars.headOption.map { _._1 }
      cars :+= (car, position)
      cars = cars.sortBy { _._2 }
      car ! Car.EnteredRoad(position, length, carAhead, endJunction)

    case LeaveRoad =>
      cars = cars.filter { _._1 != sender }

    case TimeSynchronizer.ComputeTimeSlot =>
      log.info("Computing time slot")
      sender ! TimeSynchronizer.TimeSlotComputed
  }
}
