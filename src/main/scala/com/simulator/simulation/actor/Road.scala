package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging

object Road {
  def props(startJunction: ActorRef, endJunction: ActorRef, length: Double): Props =
    Props(new Road(startJunction, endJunction, length))

  final case class GetNthCar(n: Int)
  final case class AddCar(car: ActorRef, positionX: Double) //position to simple check, whether there is no colision
  final case class RemoveCar(car: ActorRef)
  final case class NthCar(car: Option[ActorRef])
  case object GetEndJunction
  case object GetLength
  final case class GetLengthResult(length: Double)
  final case class GetEndJunctionResult(endJunction: ActorRef)
  final case class Movement(from: Double, to: Double)
}

class Road(val startJunction: ActorRef,
           val endJunction: ActorRef,
           val length: Double) extends Actor {

  import Road._

  val log = Logging(context.system, this)

  var cars = List.empty[ActorRef]
  var synchronization: Int = -1

  override def preStart() {
    log.info("Started")
  }

  def receive = {
    case GetNthCar(n) =>
      sender() ! NthCar(cars.lift(n - 1))
    case AddCar(ref, pos) => // TODO: pos
      cars :+= ref
      log.info(s"Add car ${ref.path}")
    case RemoveCar(ref) =>
      cars = cars.filter(_ != ref)
    case GetEndJunction =>
      sender() ! GetEndJunctionResult(endJunction)
    case GetLength =>
      sender() ! GetLengthResult(length)
    case Movement(from, to) => ??? // TODO
//      val index = cars.indexOf(sender())
//      val collision = cars.size match {
//        case _ if index == 0 => false
//        case _ => {
//          nextCarPosition = cars(index - 1)._2
//          if (nextCarPosition) //jesli nie kolidują to spoko, ale wpp
//          /*
//          1. gość wychodzi
//          2. gość przesuwa się
//          lista tych, którzy dokonali ruchu?
//
//          Podział na listy: osobno auta(najpierw)
//          Potem junction i roads
//
//           */
//        }
//      }
  }
}
