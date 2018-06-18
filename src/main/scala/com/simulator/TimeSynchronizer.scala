package com.simulator

import akka.actor.{Actor, ActorRef, Props}

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
    case AddObject(id) => objects :+= id
    case RemoveObject(id) => objects = objects.filter(_ != id)
  }
}