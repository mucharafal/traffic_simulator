package com.simulator.ui

import scala.collection.immutable.Seq

case class Position(x: Float, y: Float)

object Position {
  def interpolate(p1: Position, p2: Position)(alpha: Float): Position =
    Position(p1.x * alpha + p2.x * (1 - alpha), p1.y * alpha + p2.y * (1 - alpha))
}

case class Rect(x: Float, y: Float, w: Float, h: Float)

object Rect {
  def fromLTRB(left: Float, top: Float, right: Float, bottom: Float) =
    Rect(left, top, right - top, bottom - top)
}

case class Car(id: Int, road: Road, positionOnRoad: Float)

case class Junction(id: Int, position: Position)

case class Road(id: Int, start: Junction, end: Junction)

case class Snapshot(cars: Seq[Car], roads: Seq[Road], junctions: Seq[Junction])
