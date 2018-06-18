package com.simulator.ui

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.text.TextAlignment

class Visualizer(val canvas: Canvas) {

  private val roadLineWidth = 2
  private val carOvalSize = 10
  private val junctionOvalSize = 8
  private val carLabelOffset = 10

  def drawSnapshot(snapshot: Snapshot): Unit = {
    val boundingBox = calculateBoundingBox(snapshot)

    val gc = canvas.graphicsContext2D

    gc.fill = Color.White
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get)

    // roads
    gc.stroke = Color.Black
    gc.lineWidth = roadLineWidth
    for (road <- snapshot.roads) {
      gc.strokeLine(road.start.position.x, road.start.position.y, road.end.position.x, road.end.position.y)
    }

    // junctions
    gc.fill = Color.Black
    for (junction <- snapshot.junctions) {
      gc.fillOval(junction.position.x - junctionOvalSize / 2, junction.position.y - junctionOvalSize / 2,
        junctionOvalSize, junctionOvalSize)
    }

    // cars
    gc.fill = Color.Red
    for (car <- snapshot.cars) {
      val position = calculateCarPosition(car)
      gc.fillOval(position.x - carOvalSize / 2, position.y - carOvalSize / 2, carOvalSize, carOvalSize)
    }

    gc.fill = Color.Black
    gc.textAlign = TextAlignment.Center
    for (car <- snapshot.cars) {
      val position = calculateCarPosition(car)
      gc.fillText(car.id.toString, position.x, position.y - carLabelOffset)
    }
  }

  private def calculateCarPosition(car: Car): Position =
    Position.interpolate(car.road.start.position, car.road.end.position)(car.positionOnRoad)

  private def calculateBoundingBox(snapshot: Snapshot): Rect = {
    val xs = snapshot.junctions.map { _.position.x }
    val ys = snapshot.junctions.map { _.position.y }
    Rect.fromLTRB(
      left = xs.min,
      right = xs.max,
      top = ys.min,
      bottom = ys.max
    )
  }
}
