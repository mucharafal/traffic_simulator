package com.simulator

import akka.actor.{Actor, ActorRef, Props}
import Road._
import com.simulator.Car.PositionOnRoad
import com.simulator.TimeSynchronizer.{CarComputed, ComputeTimeSlot}

object Car {
  type PositionOnRoad = (ActorRef, Double)

  def props(currentPosition: PositionOnRoad,
            destinationPosition: PositionOnRoad,
            driveAlgorithm: Any): Props = // TODO: proper type
    Props(new Car(currentPosition, destinationPosition, driveAlgorithm))

  case object CarGetInformationRequest  //Get information about position, velocity, breaking signal
                                              // and number of timeslot
  final case class CarGetInformationResult(roadId: ActorRef,
                                           position_x: Double,
                                           velocity: Double,
                                           breaking: Boolean)
  case object Crash
}

class Car(currentPosition: PositionOnRoad,
          destination: PositionOnRoad,
          val driveAlgorithm: Any) extends Actor {

  import Car._
  var (roadId, position) = currentPosition
  val (destinationRoadId, destinationPosition) = destination
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
    case CarGetInformationRequest =>
      sender() ! CarGetInformationResult(roadId, position, velocity, breaking)
    case ComputeTimeSlot(s) => {
      synchronizer = s
      if(!crashed) {
        if(!started) {
          val newPosition = position + velocity + acceleration / 2
          if (newPosition - currentRoadLength > 0) {
            //nextJunction ! Turning(roadId, roadToTurnOn) TODO
            position = newPosition - currentRoadLength
            roadToTurnOn ! AddCar(self)
            roadId ! RemoveCar(self)
            roadId = roadToTurnOn
            roadId ! GetLength
            roadId ! GetEndJunction
          } else {
            if (roadId == destinationRoadId &&
              newPosition > destinationPosition) {
              roadId ! RemoveCar(self)    //end of journey
              context stop self
            }

            roadId ! Movement(position, newPosition)
            position = newPosition
          }
          velocity += acceleration / 2
//          driveAlgorithm(roadId,
//            nextJunction,
//            position,
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
      sender() ! CarComputed
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
