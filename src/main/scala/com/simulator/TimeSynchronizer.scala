package com.simulator

import akka.actor.{Props}

object TimeSynchronizer {
  def props(): Props = Props(TimeSynchronizer)
  final case class Computed(From: Int)  //computation in time slot ended
  final case class AddObject(Id: Int)
  final case class RemoveObjet(Id: Int)
  case object ComputeTimeSlot
}

class TimeSynchronizer(var objectsList: List[Int]){
  var receivedMessagesCounter: Int = 0
  def receive() = {
    case Computed(_) => {
      if (objectsList.size != 0) {
        receivedMessagesCounter = receivedMessagesCounter + 1
        if (receivedMessagesCounter == objectsList.size) {
          receviedMessagesCounter = 0
          objectsList.foreach((x: Int) => x ! ComputeTimeSlot)
        }
      }
    }
  }
}