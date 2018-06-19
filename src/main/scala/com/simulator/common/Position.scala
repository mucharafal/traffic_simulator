package com.simulator.common

object Position {
  def interpolate(p1: Position, p2: Position)(alpha: Float): Position =
    Position(p1.x * alpha + p2.x * (1 - alpha), p1.y * alpha + p2.y * (1 - alpha))
}

case class Position(x: Float, y: Float)