package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.simulator.common.RoadId

import scala.collection.mutable

object Road {
  def props(roadId: RoadId, startJunction: JunctionRef, endJunction: JunctionRef, length: Double): Props =
    Props(new Road(roadId, startJunction, endJunction, length))

  case object EnterRoad // send by Car
  case object LeaveRoad // send by Car
}

class Road(val roadId: RoadId,
           val startJunction: JunctionRef,
           val endJunction: JunctionRef,
           val length: Double) extends Actor {

  val log = Logging(context.system, this)

  val cars = mutable.Queue.empty[ActorRef]

  override def preStart() {
    log.info("Started")
  }

  override def receive = {
    case Road.EnterRoad =>
      val car = sender
      val carAhead = cars.lastOption
      cars += car
      car ! Car.EnteredRoad(length, carAhead, endJunction)

    case Road.LeaveRoad =>
      assert(cars.head == sender)
      cars.dequeue()
      cars.headOption.foreach { _ ! Car.CarAheadChanged(None) }

    case TimeSynchronizer.ComputeTimeSlot =>
      sender ! TimeSynchronizer.TimeSlotComputed
  }
}
