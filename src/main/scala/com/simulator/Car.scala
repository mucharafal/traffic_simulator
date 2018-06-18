package com.simulator

import akka.actor.{Props, Actor, ActorRef}
import Road._

object Car {
  def props(currentPosition:(ActorRef, Double),
            destinationPosition:(ActorRef, Double),
            driveAlgorithm: Any): Props =
    Props(new Car(currentPosition, destinationPosition, driveAlgorithm))

  case object CarGetInformationRequest  //Get information about position, velocity, breaking signal
                                              // and number of timeslot
  final case class CarGetInformationResult(roadId: ActorRef,
                                           position_x: Double,
                                           velocity: Double,
                                           breaking: Boolean)
}

class Car(currentPosition:(ActorRef, Double),
          destinationPosition:(ActorRef, Double),
          val driveAlgorithm: Any) extends Actor {

  import Car._
  var (roadId, positionX) = currentPosition
  val (destinationRoadId, destinationPositionX) = destinationPosition
  //what to do in time slot
  var turnToRoad: ActorRef = null
  var acceleration: Double = 0
  var velocity: Double = 0
  var breaking: Boolean = false
  var synchronizer: Int = -1
  var current_road_length = (roadId ! GetLength)

  def receive = {
    case CarGetInformationRequest =>
      sender() ! CarGetInformationResult(roadId, positionX, velocity, breaking)
    case ComputeTimeSlot(s) => {
      new_position = positionX + velocity + acceleration / 2
      if (new_position - current_road_length > 0) {

      }
    }
  }

}