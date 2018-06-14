package com.simulator

import akka.actor.{Props}

object Junction {
  def props(): Props = Props(Road)
  final case class RideTo(carId: Int, roadFromId: Int, roadToId: Int)
}
