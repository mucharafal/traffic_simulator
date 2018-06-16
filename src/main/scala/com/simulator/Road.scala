package com.simulator

import akka.actor.{Props}

object Road {
  def props(): Props = Props(Road)
  final case class GetCarIdFirstCar(From: Int)
  final case class AddCar(From: Int, positionX: Double)   //position to simple check, wheather there is no colision
  final case class RemoveCar(From: Int)
}

class Road(val roadId: Int, val supervisorId: Int,
           val junctionStartId: Int, val junctionToId: Int) {

}
