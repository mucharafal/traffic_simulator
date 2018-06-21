package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging

import scala.collection.immutable.Seq

object TimeSynchronizer {
  def props(): Props = Props(new TimeSynchronizer(Seq.empty, Seq.empty))

  final case class ComputeTimeSlot(n: Int) //computation in time slot ended
  final case class AddCar(id: ActorRef)
  final case class RemoveCar(id: ActorRef)
  final case class AddInfrastructure(id: ActorRef)
  final case class RemoveInfrastructure(id: ActorRef)
  case object CarComputed
  case object InfrastructureComputed
  case object Start
}

class TimeSynchronizer(var cars: Seq[ActorRef], var infrastructure: Seq[ActorRef]) extends Actor {
  import TimeSynchronizer._

  val log = Logging(context.system, this)

  var sentMessagesCounter = 0         //number of sending turns since start synchronizer
                                      //using to synchronize turns
  var receivedMessagesFromCarsCounter = 0
  var receivedMessagesFromInfrastructureCounter = 0

  var started: Boolean = false

  def receive = {
    case CarComputed =>
      started = true
      val carRef = sender()
      if (cars.contains(carRef) && !cars.isEmpty) {
        receivedMessagesFromCarsCounter += 1
        if (receivedMessagesFromCarsCounter == cars.size) {
          receivedMessagesFromCarsCounter = 0
          infrastructure.foreach(_ ! ComputeTimeSlot(sentMessagesCounter))
        }
      }
    case InfrastructureComputed =>
      val infrastructureRef = sender()
      if (infrastructure.contains(infrastructureRef) && !infrastructure.isEmpty) {
        receivedMessagesFromInfrastructureCounter += 1
        if (receivedMessagesFromInfrastructureCounter == infrastructure.size) {
          receivedMessagesFromInfrastructureCounter = 0
          sentMessagesCounter += 1
          sentMessagesCounter %= 1000000
          cars.foreach(_ ! ComputeTimeSlot(sentMessagesCounter))
        }
      }
    case AddCar(id) =>
      cars :+= id
      log.info(s"Added object ${id.path}")
    case RemoveCar(id) => cars = cars.filter(_ != id)
    case AddInfrastructure(id) => infrastructure :+= id
    case RemoveInfrastructure(id) => infrastructure = infrastructure.filter(_ != id)
    case Start =>
      if(!started) {
        cars.foreach(_ ! ComputeTimeSlot(sentMessagesCounter))
      }
  }
}