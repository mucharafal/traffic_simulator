package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging

import scala.collection.immutable.Seq

object TimeSynchronizer {
  def props(): Props = Props(new TimeSynchronizer(Seq.empty))

  final case class ComputeTimeSlot(n: Int) //computation in time slot ended
  final case class AddObject(id: ActorRef)
  final case class RemoveObject(id: ActorRef)
  case object Computed
}

class TimeSynchronizer(var objects: Seq[ActorRef]) extends Actor {
  import TimeSynchronizer._

  val log = Logging(context.system, this)

  var sentMessagesCounter = 0
  var receivedMessagesCounter = 0

  def receive = {
    case Computed =>
      val clientRef = sender()
      if (objects.nonEmpty && objects.contains(clientRef)) {
        receivedMessagesCounter += 1
        if (receivedMessagesCounter == objects.size) {
          receivedMessagesCounter = 0
          objects.foreach(_ ! ComputeTimeSlot(sentMessagesCounter))
          sentMessagesCounter += 1
          sentMessagesCounter %= 1000000
        }
      }
    case AddObject(id) =>
      objects :+= id
      log.info(s"Added object ${id.path}")

    case RemoveObject(id) => objects = objects.filter(_ != id)
  }
}