package com.simulator.common

import scala.math.sqrt

case class Vec2D(x: Double, y: Double) {
  def +(other: Vec2D) = Vec2D(x + other.x, y + other.y)
  def *(alpha: Double) = Vec2D(x * alpha, y * alpha)
  def /(alpha: Double) = Vec2D(x / alpha, y / alpha)
  def unary_- = this * -1.0
  def -(other: Vec2D) = this + (-other)
  def length = sqrt(x * x + y * y)

  def normalized = this / length

  def interpolate(other: Vec2D) = (alpha: Double) =>
    this * (1 - alpha) + other * alpha

  def rotated90DegreesCW = Vec2D(-y, x)
  def rotated90DegreesCCW = -rotated90DegreesCW
}
