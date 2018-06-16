package com.simulator

import scala.collection.mutable.LinkedList
import akka.actor.{Props, Actor, ActorRef}

object Road {
  def props(key: ActorRef, supervisor: ActorRef, junctionStart: ActorRef, junctionEnd: ActorRef): Props =
    Props(new Road(key, supervisor, junctionStart, junctionEnd))
  final case class GetCarIdFirstCar(From: ActorRef)
  final case class AddCar(From: ActorRef, positionX: Double)   //position to simple check, wheather there is no colision
  final case class RemoveCar(From: ActorRef)
}

class Road(val roadId: ActorRef, val supervisorId: ActorRef,
           val junctionStartId: ActorRef, val junctionToId: ActorRef) extends Actor {
  import Road._
  var CarsList = new LinkedList[ActorRef]()
  def receive = {
    case GetCarIdFirstCar(from) => {
      CarsList.size match {
        case 0 =>
          from ! null
        case _ =>
          from ! CarsList.get(0)
      }
    }
    case AddCar(ref, pos) =>
      CarsList = CarsList ++ List(ref)
    case RemoveCar(ref) =>
      CarsList = CarsList.filter((x: ActorRef) => x != ref)
  }

}
