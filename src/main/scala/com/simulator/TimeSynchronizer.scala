package com.simulator

import akka.actor.{Props, ActorRef, Actor}
import scala.collection.mutable.LinkedList

object TimeSynchronizer {
  def props(): Props = Props(new TimeSynchronizer(new LinkedList[ActorRef]()))
  final case class Computed()  //computation in time slot ended
  final case class AddObject(Id: ActorRef)
  final case class RemoveObject(Id: ActorRef)
  case object ComputeTimeSlot
}

class TimeSynchronizer(var objectsList: LinkedList[ActorRef]) extends Actor{
  import TimeSynchronizer._
  var receivedMessagesCounter: Int = 0
  def receive = {
    case Computed() => {
      val clientRef = sender()
      if (objectsList.size != 0 && objectsList.exists(x => x == clientRef)) {
        receivedMessagesCounter = receivedMessagesCounter + 1
        if (receivedMessagesCounter == objectsList.size) {
          receivedMessagesCounter = 0
          objectsList.foreach((x: ActorRef) => x ! ComputeTimeSlot)
        }
      }
    }
    case AddObject(id) => objectsList = objectsList ++ List(id)
    case RemoveObject(id) => objectsList = objectsList.filter((x: ActorRef) => x != id)
  }
}