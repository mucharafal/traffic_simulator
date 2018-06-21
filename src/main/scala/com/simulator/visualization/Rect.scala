package com.simulator.visualization

object Rect {
  def fromLTRB(left: Double, top: Double, right: Double, bottom: Double) =
    Rect(left, top, right - left, bottom - top)
}

case class Rect(x: Double, y: Double, w: Double, h: Double)