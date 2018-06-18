package com.simulator

import scala.collection.mutable.LinkedList
import akka.actor.{Props, Actor, ActorRef}

object Road {
  def props(supervisor: ActorRef, junctionStart: ActorRef, junctionEnd: ActorRef): Props =
    Props(new Road(supervisor, junctionStart, junctionEnd))
  final case class GetCarIdNthCar(N: Int)
  final case class AddCar(CarRef: ActorRef, positionX: Double)   //position to simple check, whether there is no colision
  final case class RemoveCar(CarRef: ActorRef)
  case object NoCar
  final case class CarRef(Ref: ActorRef)
}

class Road(val supervisorId: ActorRef,
           val junctionStartId: ActorRef, val junctionToId: ActorRef) extends Actor {
  import Road._
  var CarsList = new LinkedList[ActorRef]()
  def receive = {
    case GetCarIdNthCar(n) => {
      CarsList.get(n-1) match {
        case None =>
          sender() ! NoCar
        case Some(ref) =>
          sender() ! CarRef(ref)
      }
    }
    case AddCar(ref, pos) =>
      CarsList = CarsList ++ List(ref)
    case RemoveCar(ref) =>
      CarsList = CarsList.filter((x: ActorRef) => x != ref)
  }
}
