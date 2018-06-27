package com.simulator

import scala.concurrent.duration._
import scala.language.postfixOps

object Config {
  val worldSize = 10
  val junctionCount = 20
  val carCount = 50

  val clockInterval = 25 milli

  val carSpeed = 0.25
  val carDistance = 0.5
}
