package com.simulator

import scala.collection.mutable.LinkedList
import akka.actor.{Props, Actor, ActorRef}

object Road {
  def props(supervisor: ActorRef, junctionStart: ActorRef, junctionEnd: ActorRef, length: Double): Props =
    Props(new Road(supervisor, junctionStart, junctionEnd, length))
  final case class GetCarIdNthCar(N: Int)
  final case class AddCar(CarRef: ActorRef, positionX: Double)   //position to simple check, whether there is no colision
  final case class RemoveCar(CarRef: ActorRef)
  case object NoCar
  final case class CarRef(Ref: ActorRef)
  case object GetJunction
  case object GetLength
  final case class GetLengthResult(Length: Double)
  final case class GetJunctionResult(JunctionId: ActorRef)
  final case class Movement(from: Double, to: Double)
}

class Road(val supervisorId: ActorRef,
           val junctionStartId: ActorRef,
           val junctionToId: ActorRef,
           val length: Double) extends Actor {
  import Road._
  var CarsList = List.empty[ActorRef]
  var synchronization: Int = -1
  def receive = {
    case GetCarIdNthCar(n) => {
      CarsList.lift(n-1) match {
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
    case GetJunction =>
      sender() ! GetJunctionResult(junctionToId)
    case GetLength =>
      sender() ! GetLengthResult(length)
    case Movement(from, to) => ??? // TODO
//      val index = CarsList.indexOf(sender())
//      val collision = CarsList.size match {
//        _ if index == 0 => false
//        _ => {
//        nextCarPosition = CarsList(index-1)._2
//        if(nextCarPosition)//jesli nie kolidują to spoko, ale wpp
//        /*
//        1. gość wychodzi
//        2. gość przesuwa się
//        lista tych, którzy dokonali ruchu?
//
//        Podział na listy: osobno auta(najpierw)
//        Potem junction i roads
//
//         */
//      }
//      }
  }
}
