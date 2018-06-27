package com.simulator.visualization

import com.simulator.common._
import CollectionExtensions._
import javafx.geometry.Point2D
import javafx.scene.transform.{Scale, Translate}
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, TextAlignment}
import scalaz.Memo

class VisualizationServiceImpl(val canvas: Canvas) extends VisualizationService {

  private val canvasPadding = 50
  private val roadLineWidth = 4
  private val roadsDistance = 5
  private val carOvalSize = 6
  private val junctionOvalSize = 12
  private val carLabelOffset = 10

  override def visualize(snapshot: Snapshot): Unit = {
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

        val scale = math.min(screenW / worldW, screenH / worldH)
        new Scale(scale, scale)
      },
      // 1. Move center to point (0, 0)
      {
        val worldCenterX = boundingBox.x + boundingBox.w / 2
        val worldCenterY = boundingBox.y + boundingBox.h / 2
        new Translate(-worldCenterX, -worldCenterY)
      }
    ).reduce { _.createConcatenation(_) }

    val worldToScreen: Vec2D => Vec2D =
      Memo.immutableHashMapMemo { worldToScreenTransform.transform(_) }

    val junctions = snapshot.junctions.keyBy { _.id }
    val roads = snapshot.roads.keyBy { _.id }
    val cars = snapshot.cars.keyBy { _.id }

    val roadLength: RoadId => Double = Memo.immutableHashMapMemo { roadId =>
      val road = roads(roadId)
      val startJunction = junctions(road.start)
      val endJunction = junctions(road.end)
      (endJunction.position - startJunction.position).length
    }

    val roadPosition: RoadId => (Vec2D, Vec2D) = Memo.immutableHashMapMemo { roadId =>
      val road = roads(roadId)
      val startJunction = junctions(road.start)
      val endJunction = junctions(road.end)
      val startPosition = worldToScreen(startJunction.position)
      val endPosition = worldToScreen(endJunction.position)
      val direction = (endPosition - startPosition).normalized
      val offset = direction.rotated90DegreesCW * (roadsDistance / 2.0)

      (startPosition + offset + direction * junctionOvalSize,
        endPosition + offset - direction * junctionOvalSize)
    }

    val carPosition: CarId => Vec2D = Memo.immutableHashMapMemo { carId =>
      val car = cars(carId)
      val (roadStart, roadEnd) = roadPosition(car.road)
      (roadStart interpolate roadEnd) (car.positionOnRoad / roadLength(car.road))
    }

    val gc = canvas.graphicsContext2D

    gc.getTransform

    gc.fill = Color.White
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get)

    // roads
    gc.lineWidth = roadLineWidth
    for (road <- roads.values) {
      val (startPosition, endPosition) = roadPosition(road.id)
      val midPosition = (startPosition interpolate endPosition) (0.8)
      val endJunction = junctions(road.end)

      gc.stroke = Color.Black
      gc.strokeLine(startPosition.x, startPosition.y, endPosition.x, endPosition.y)

      gc.stroke =
        if (endJunction.greenLightRoad.contains(road.id))
          Color.Green
        else
          Color.Black
      gc.strokeLine(midPosition.x, midPosition.y, endPosition.x, endPosition.y)
    }

    // junctions
    gc.fill = Color.Grey
    for (junction <- snapshot.junctions) {
      val pos = worldToScreen(junction.position)
      gc.fillOval(pos.x - junctionOvalSize / 2, pos.y - junctionOvalSize / 2, junctionOvalSize, junctionOvalSize)
    }

    // cars
    gc.fill = Color.Red
    for (car <- snapshot.cars) {
      val pos = carPosition(car.id)
      gc.fillOval(pos.x - carOvalSize / 2, pos.y - carOvalSize / 2, carOvalSize, carOvalSize)
    }

    gc.fill = Color.MediumBlue
    gc.font = new Font(12.0)
    gc.textAlign = TextAlignment.Center
    for (car <- snapshot.cars) {
      val pos = carPosition(car.id)
      val text = f"${ car.id.value }"
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

  implicit def point2DToVec2D(p: Point2D): Vec2D = Vec2D(p.getX, p.getY)
  implicit def vec2DToPoint2D(p: Vec2D): Point2D = new Point2D(p.x, p.y)
}
