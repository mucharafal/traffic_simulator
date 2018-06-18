package com.simulator.ui

import scala.collection.immutable.Seq

case class Position(x: Float, y: Float) {
  def interpolate(other: Position)(alpha: Float): Position =
    Position(x * alpha + other.x * (1 - alpha), y * alpha + other.y * (1 - alpha))
}

case class Rect(x: Float, y: Float, w: Float, h: Float)

object Rect {
  def fromLTRB(left: Float, top: Float, right: Float, bottom: Float) =
    Rect(left, top, right - top, bottom - top)
}

case class Car(id: Int, position: Position)

case class Junction(id: Int, position: Position)

case class Road(id: Int, start: Junction, end: Junction)

case class Snapshot(cars: Seq[Car], roads: Seq[Road], junctions: Seq[Junction]) {

  def getBoundingBox: Rect = {
    val xs = junctions.map { _.position.x }
    val ys = junctions.map { _.position.y }
    Rect.fromLTRB(
      left = xs.min,
      right = xs.max,
      top = ys.min,
      bottom = ys.max
    )
  }
}