package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.simulator.common.RoadId
import com.simulator.simulation.actor.TimeSynchronizer.ComputeTimeSlot

object Road {
  def props(roadId: RoadId, startJunction: ActorRef, endJunction: ActorRef, length: Double): Props =
    Props(new Road(roadId, startJunction, endJunction, length))

  final case class GetNthCar(n: Int)
  final case class AddCar(car: ActorRef, time: Double, to: Double)
  final case class RemoveCar(car: ActorRef)
  final case class NthCar(car: Option[ActorRef])
  case object GetEndJunction
  case object GetLength
  final case class GetLengthResult(length: Double)
  final case class GetEndJunctionResult(endJunction: ActorRef)
  final case class Movement(from: Double, to: Double)

  case object GetCars
  case class GetCarsResult(cars: List[CarRef])
}

class Road(val roadId: RoadId,
           val startJunction: ActorRef,
           val endJunction: ActorRef,
           val length: Double) extends Actor {

  import Car._
  import Road._

  val log = Logging(context.system, this)

  var cars = List.empty[ActorRef]
  var synchronization: Int = -1

  var movementsInTurn = List.empty[(CarRef, Double, Double)]
  var addedInTurn = List.empty[(CarRef, Double, Double)]

  override def preStart() {
    log.info("Started")
  }

  override def receive = {
    case GetNthCar(n) =>
      sender() ! NthCar(cars.lift(n - 1))

    case AddCar(car, time, position) =>
      cars :+= car
      addedInTurn :+= (car, time, position)
      log.info(s"Add car ${ car.path }")

    case RemoveCar(ref) =>
      cars = cars.filter(_ != ref)

    case GetEndJunction =>
      sender() ! GetEndJunctionResult(endJunction)

    case GetLength =>
      sender() ! GetLengthResult(length)

    case Movement(from, to) =>
      movementsInTurn :+= (sender(), from, to)

    case ComputeTimeSlot(s) =>
      log.info("Computing time slot")

      sender ! TimeSynchronizer.InfrastructureComputed

//      synchronization = s
//
//      addedInTurn = addedInTurn.sortBy(_._2)
//      var maxPosition: Double = 0.0
//      for (i <- 1 until movementsInTurn.size) {
//        val carA = movementsInTurn(i - 1)
//        val carB = movementsInTurn(i)
//
//        if (carA._3 > carB._3) {
//          carA._1 ! Crash
//          carB._1 ! Crash
//        }
//
//        maxPosition = math.max(math.max(carA._3, carB._3), maxPosition)
//      }
//
//      if (addedInTurn.nonEmpty) {
//        val max: (ActorRef, Double, Double) = addedInTurn.filter(_ == maxPosition)(0) // TODO
//        movementsInTurn :+= (max._1, 0.0, max._3)
//      }
//
//      movementsInTurn = movementsInTurn.sortBy(_._2)
//      for (i <- 1 until movementsInTurn.size) {
//        val carA = movementsInTurn(i - 1)
//        val carB = movementsInTurn(i)
//
//        if (carA._3 > carB._3) {
//          carA._1 ! Crash
//          carB._1 ! Crash
//        }
//      }

  }
}
