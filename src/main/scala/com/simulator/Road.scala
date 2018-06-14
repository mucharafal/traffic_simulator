package com.simulator

import akka.actor.{Props}

object Road {
  def props(): Props = Props(Road)
  final case class PositionRequest(carId: Int)
  final case class GetInto(carId: Int)

}
