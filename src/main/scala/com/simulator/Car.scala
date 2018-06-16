package com.simulator

import akka.actor.{Props}

object Car {
  def props(): Props = Props(Car)

  final case class GetInformationRequest(From: Int)  //Get information about position, velocity, breaking signal
                                              // and number of timeslot
  final case class GetInformationResult(From: Int, roadId: Int, position_x: Double, velocity: Double)
}

class Car(val carId: Int, val supervisorId: Int,
          currentPosition:(Road, Double),
          destiantionPosition:(Road, Double),
          var velocity: Double, var vabreaking: Boolean, val driveAlgorithm) {

  var (roadId, positionX) = currentPosition
  val (destinationRoadId, destinationPositionX) = destiantionPosition

  def receive = {
    case GetInformationRequest(From) =>
      From ! GetInormationResult(carId, roadId, positionX)
  }
}