package com.simulator.visualization

object Rect {
  def fromLTRB(left: Float, top: Float, right: Float, bottom: Float) =
    Rect(left, top, right - left, bottom - top)
}

case class Rect(x: Float, y: Float, w: Float, h: Float)