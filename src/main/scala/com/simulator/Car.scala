package com.simulator

import akka.actor.{Props}

object Car {
  def props(): Props = Props(Car)
  final case class PositionRequest()
  final case class GetInto

}