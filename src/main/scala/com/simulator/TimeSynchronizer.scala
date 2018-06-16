package com.simulator

import akka.actor.{Props}

object TimeSynchronizer {
  def props(): Props = Props(TimeSynchronizer)
  final case class Computed(From: Int)  //computation in time slot ended
  final case class GenerateKey(From: Int)
  final case class AddObject(Id: Int)
  final case class RemoveObjet(Id: Int)
  case object ComputeTimeSlot
}

class TimeSynchronizer(var objectsList: List[Int]){

}