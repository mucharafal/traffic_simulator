package com.simulator

import akka.actor.{Props, Actor, ActorRef}
import Road._

object Car {
  def props(car: Car): Props = Props(car)

  final case class GetInformationRequest()  //Get information about position, velocity, breaking signal
                                              // and number of timeslot
  final case class GetInformationResult(roadId: ActorRef, position_x: Double, velocity: Double)
}

class Car(val supervisorId: ActorRef,
          currentPosition:(ActorRef, Double),
          destinationPosition:(ActorRef, Double),
          var velocity: Double, var breaking: Boolean, val driveAlgorithm: Any) extends Actor {

  import Car._
  var (roadId, positionX) = currentPosition
  val (destinationRoadId, destinationPositionX) = destinationPosition

  def receive = {
    case GetInformationRequest() =>
      sender() ! GetInformationResult(roadId, positionX, velocity)
  }
}