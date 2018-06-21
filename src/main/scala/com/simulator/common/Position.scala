package com.simulator.common

import scala.math.{pow, sqrt}

object Position {
  def distance(p1: Position, p2: Position) =
    sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2))

  def interpolate(p1: Position, p2: Position)(alpha: Double): Position =
    Position(p1.x * alpha + p2.x * (1 - alpha), p1.y * alpha + p2.y * (1 - alpha))
}

case class Position(x: Double, y: Double)