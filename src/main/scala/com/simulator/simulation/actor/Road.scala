package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.simulator.common.RoadId

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

  val log = Logging(context.system, this)

  var cars = List.empty[ActorRef]

  override def preStart() {
    log.info("Started")
  }

  override def receive = {
    case Road.EnterRoad(position) =>
      val car = sender
      val carAhead = cars.headOption
      cars ::= car
      car ! Car.EnteredRoad(position, length, carAhead, endJunction)

    case Road.LeaveRoad =>
      cars = cars.filter { _ != sender }

    case TimeSynchronizer.ComputeTimeSlot =>
      sender ! TimeSynchronizer.TimeSlotComputed
  }
}
