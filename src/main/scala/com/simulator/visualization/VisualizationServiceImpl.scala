package com.simulator.visualization

import com.simulator.common._
import com.simulator.util.CollectionExtensions._
import javafx.scene.transform.{Scale, Translate}
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.text.TextAlignment

class VisualizationServiceImpl(val canvas: Canvas) extends VisualizationService {

  private val canvasPadding = 50
  private val roadLineWidth = 2
  private val roadsDistance = 4
  private val carOvalSize = 10
  private val junctionOvalSize = 8
  private val carLabelOffset = -40

  override def visualize(snapshot: Snapshot): Unit = {
    val junctionMap: Map[JunctionId, JunctionState] = snapshot.junctions.keyBy { _.id }
    val roadMap: Map[RoadId, RoadState] = snapshot.roads.keyBy { _.id }

    def getCarPosition(car: CarState): Vec2D = {
      val road = roadMap(car.road)
      (junctionMap(road.start).position interpolate junctionMap(road.end).position)(car.positionOnRoad)
    }

    val boundingBox = calculateBoundingBox(snapshot.junctions)

    val worldToScreenTransform = Seq(
      // 3. Move point (0, 0) to the center of the canvas
      new Translate(canvas.width.get / 2, canvas.height.get / 2),
      // 2. Scale to fit canvas minus padding
      {
        val screenW = canvas.width.get - 2 * canvasPadding
        val screenH = canvas.height.get - 2 * canvasPadding
        val worldW = boundingBox.w
        val worldH = boundingBox.h

        val scale = Math.min(screenW / worldW, screenH / worldH)
        new Scale(scale, scale)
      },
      // 1. Move center to point (0, 0)
      {
        val worldCenterX = boundingBox.x + boundingBox.w / 2
        val worldCenterY = boundingBox.y + boundingBox.h / 2
        new Translate(-worldCenterX, -worldCenterY)
      }
    ).reduce { _.createConcatenation(_) }

    // Map world position to screen position
    def worldToScreen(pos: Vec2D) = {
      val point = worldToScreenTransform.transform(pos.x, pos.y)
      Vec2D(point.getX, point.getY)
    }

    val gc = canvas.graphicsContext2D

    gc.getTransform

    gc.fill = Color.White
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get)

    // roads
    gc.lineWidth = roadLineWidth
    for (road <- snapshot.roads) {
      val startJunction = junctionMap(road.start)
      val endJunction = junctionMap(road.end)
      var start = worldToScreen(startJunction.position)
      var end = worldToScreen(endJunction.position)

      val offset = (end - start).normalized.rotated90DegreesCCW * (roadsDistance / 2)
      start += offset
      end += offset

        gc.stroke =
        if (endJunction.greenLightRoad.contains(road.id))
          Color.Green
        else
          Color.Black
      gc.strokeLine(start.x, start.y, end.x, end.y)
    }

    // junctions
    gc.fill = Color.Black
    for (junction <- snapshot.junctions) {
      val pos = worldToScreen(junction.position)
      gc.fillOval(pos.x - junctionOvalSize / 2, pos.y - junctionOvalSize / 2, junctionOvalSize, junctionOvalSize)
    }

    // cars
    gc.fill = Color.Red
    for (car <- snapshot.cars) {
      val pos = worldToScreen(getCarPosition(car))
      gc.fillOval(pos.x - carOvalSize / 2, pos.y - carOvalSize / 2, carOvalSize, carOvalSize)
    }

    gc.fill = Color.Black
    gc.textAlign = TextAlignment.Center
    for (car <- snapshot.cars) {
      val pos = worldToScreen(getCarPosition(car))
      val text = f"car #${ car.id.value }\nspeed=${ car.velocity }\nbreaking=${ car.breaking }"
      gc.fillText(text, pos.x, pos.y - carLabelOffset)
    }
  }

  private def calculateBoundingBox(junctions: Iterable[JunctionState]): Rect = {
    val xs = junctions.map { _.position.x }
    val ys = junctions.map { _.position.y }
    Rect.fromLTRB(
      left = xs.min,
      right = xs.max,
      top = ys.min,
      bottom = ys.max
    )
  }

  implicit def position2Vec2D(position: Position): Vec2D = Vec2D(position.x, position.y)
}
