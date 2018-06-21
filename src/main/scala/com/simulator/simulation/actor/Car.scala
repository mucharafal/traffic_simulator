package com.simulator.simulation.actor

import akka.actor.{Actor, Props}
import akka.event.Logging
import com.simulator.common.CarId
import com.simulator.simulation.actor.Road._
import akka.pattern.ask

object Car {
  def props(carId: CarId, road: RoadRef, positionOnRoad: Double): Props =
    Props(new Car(carId, road, positionOnRoad))

  case object GetState
  final case class GetStateResult(carId: CarId,
                                  roadRef: RoadRef,
                                  positionOnRoad: Double,
                                  velocity: Double,
                                  breaking: Boolean)
  case object Crash
}

class Car(carId: CarId, initialRoad: RoadRef, initialPosition: Double) extends Actor {

  val log = Logging(context.system, this)

  var road = initialRoad
  var position = initialPosition
  //what to do in time slot
  var roadToTurnOn: RoadRef = null
  var nextJunction: JunctionRef = null
  var acceleration: Double = 0
  var velocity: Double = 0
  var breaking: Boolean = false
  var currentRoadLength: Double = 1000000
  var crashed: Boolean = false
  var crashedCounter: Int = 10
  var started: Boolean = false

  road ! GetLength
  road ! GetEndJunction

  override def preStart() {
    log.info("Started")
  }

  def receive = {
    case Car.GetState =>
      sender() ! Car.GetStateResult(carId, road, position, velocity, breaking)

    case TimeSynchronizer.ComputeTimeSlot(_) =>
      log.info("Computing time slot")



//      if (!crashed) {
//        if (!started) {
//          val distance = velocity + acceleration / 2
//          val newPosition = position + distance
//          if (newPosition - currentRoadLength > 0) {
//            //nextJunction ! Turning(roadId, roadToTurnOn) TODO
//            position = newPosition - currentRoadLength
//            roadToTurnOn ! AddCar(self, distance / position, position)
//            road ! RemoveCar(self)
//            road = roadToTurnOn
//            road ! GetLength
//            road ! GetEndJunction
//          } else {
//            road ! Movement(position, newPosition)
//            position = newPosition
//          }
//          velocity += acceleration / 2
//        } else {
//          //jakos wykryj, czy mozesz sie bezkolizyjnie wlaczyc
//        }
//      } else {
//        crashedCounter -= 1
//        if (crashedCounter == 0) {
//          road ! RemoveCar(self)
//          context stop self
//        }
//      }

      position += 0.01

      sender ! TimeSynchronizer.CarComputed


    case Road.GetLengthResult(length) =>
      assert(sender == road)
      currentRoadLength = length

    case Road.GetEndJunctionResult(endJunction) =>
      assert(sender == road)
      nextJunction = endJunction

    case Car.Crash =>
      crashed = true
      velocity = 0
  }
}
