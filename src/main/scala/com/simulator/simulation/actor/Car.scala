package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import com.simulator.simulation.actor.Car.PositionOnRoad
import com.simulator.simulation.actor.Road._
import com.simulator.simulation.actor.TimeSynchronizer.ComputeTimeSlot

object Car {
  type PositionOnRoad = (ActorRef, Double)

  def props(currentPosition: PositionOnRoad,
            destinationPosition: PositionOnRoad,
            driveAlgorithm: Any): Props = // TODO: proper type
    Props(new Car(currentPosition, destinationPosition, driveAlgorithm))

  case object GetStatus  //Get information about position, velocity, breaking signal and number of timeslot
  final case class GetStatusResult(roadId: ActorRef,
                                   position_x: Double,
                                   velocity: Double,
                                   breaking: Boolean)
  case object Crash
}

class Car(currentPosition: PositionOnRoad,
          destinationPosition: PositionOnRoad,
          val driveAlgorithm: Any) extends Actor {

  import Car._
  var (roadId, positionX) = currentPosition
  val (destinationRoadId, destinationPositionX) = destinationPosition
  //what to do in time slot
  var roadToTurnOn: ActorRef = null
  var nextJunction: ActorRef = null
  var acceleration: Double = 0
  var velocity: Double = 0
  var breaking: Boolean = false
  var synchronizer: Int = -1
  var currentRoadLength: Double = 1000000
  var crashed: Boolean = false
  var crashedCounter: Int = 10
  var started: Boolean = false
  roadId ! GetLength
  roadId ! GetEndJunction


  def receive = {
    case GetStatus =>
      sender() ! GetStatusResult(roadId, positionX, velocity, breaking)
    case ComputeTimeSlot(s) => {
      if(!crashed) {
        if(!started) {
          val newPosition = positionX + velocity + acceleration / 2
          if (newPosition - currentRoadLength > 0) {
            //nextJunction ! Turning(roadId, roadToTurnOn) TODO
            positionX = newPosition - currentRoadLength
            roadToTurnOn ! AddCar(self, positionX)
            roadId ! RemoveCar(self)
            roadId = roadToTurnOn
            roadId ! GetLength
            roadId ! GetEndJunction
          } else {
            if (roadId == destinationRoadId &&
              newPosition > destinationPositionX) {
              roadId ! RemoveCar(self)
              context stop self
            }

            roadId ! Movement(positionX, newPosition)
            positionX = newPosition
          }
          velocity += acceleration / 2
//          driveAlgorithm(roadId,
//            nextJunction,
//            positionX,
//            velocity) match {
//            case (newRoad: ActorRef, newAcc: Double) =>
//              roadToTurnOn = newRoad
//              acceleration = newAcc
//            case _ => 2
//          } // TODO
        } else {
          //jakos wykryj, czy mozesz sie bezkolizyjnie wlaczyc
        }
      } else {
        crashedCounter -= 1
        if(crashedCounter == 0){
          roadId ! RemoveCar(self)
          context stop self
        }
      }
    }
    case GetLengthResult(length) =>
      if(sender() == roadId){
        currentRoadLength = length
      }
    case GetEndJunctionResult(junctionId) =>
      if(sender() == roadId){
        nextJunction = junctionId
      }
    case Crash => {
      crashed = true
      velocity = 0
    }
  }
}