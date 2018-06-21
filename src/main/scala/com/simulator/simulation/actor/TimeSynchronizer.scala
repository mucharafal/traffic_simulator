package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging

object TimeSynchronizer {
  def props(): Props = Props(new TimeSynchronizer())

  final case class ComputeTimeSlot(n: Int) //computation in time slot ended
  final case class AddCar(id: CarRef)
  final case class RemoveCar(id: CarRef)
  final case class AddInfrastructure(id: ActorRef)
  final case class RemoveInfrastructure(id: ActorRef)
  case object CarComputed
  case object InfrastructureComputed

  case object NextTimeSlot
  case object TimeSlotComputed
}

class TimeSynchronizer extends Actor {

  val log = Logging(context.system, this)

  var cars: Set[CarRef] = Set.empty
  var infrastructure: Set[ActorRef] = Set.empty // junctions and roads

  var waitingCars: Set[CarRef] = Set.empty
  var waitingInfrastructure: Set[ActorRef] = Set.empty

  var timeSlot = 0

  var nextTimeSlotSender: ActorRef = _

  def receive = {
    case TimeSynchronizer.NextTimeSlot =>
      log.info("Next time slot")
      require(waitingCars.isEmpty)
      require(waitingInfrastructure.isEmpty)

      nextTimeSlotSender = sender
      waitingCars = cars
      waitingCars.foreach { _ ! TimeSynchronizer.ComputeTimeSlot(timeSlot) }

    case TimeSynchronizer.CarComputed =>
      log.info(s"${sender.path} computed")
      require(waitingCars.contains(sender))
      require(waitingInfrastructure.isEmpty)
      waitingCars -= sender

      if (waitingCars.isEmpty) {
        waitingInfrastructure = infrastructure
        waitingInfrastructure.foreach { _ ! TimeSynchronizer.ComputeTimeSlot(timeSlot) }
      }

    case TimeSynchronizer.InfrastructureComputed =>
      log.info(s"${sender.path} computed")
      require(waitingCars.isEmpty, sender.path)
      require(waitingInfrastructure.contains(sender))
      waitingInfrastructure -= sender

      if (waitingInfrastructure.isEmpty) {
        timeSlot += 1
        nextTimeSlotSender ! TimeSynchronizer.TimeSlotComputed
      }

    case TimeSynchronizer.AddCar(car) =>
      cars += car
      log.info(s"Added car ${ car.path }")

    case TimeSynchronizer.RemoveCar(car) =>
      cars -= car

    case TimeSynchronizer.AddInfrastructure(car) =>
      infrastructure += car

    case TimeSynchronizer.RemoveInfrastructure(car) =>
      infrastructure -= car
  }

}