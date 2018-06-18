package com.simulator.ui

import akka.actor.ActorSystem
import com.simulator.roadgenerator.RoadGenerator
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color
import scalafx.scene.text.TextAlignment

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object App extends JFXApp {

  private val canvas = new Canvas(100, 100)

  stage = new PrimaryStage {
    title = "Traffic Simulator"
    scene = new Scene {
      root = new HBox() {
        children = Seq(canvas)

        canvas.width <== this.width
        canvas.height <== this.height
      }

      fill = Color.Black
    }
  }

  private val roadLineWidth = 2
  private val carOvalSize = 10
  private val junctionOvalSize = 8
  private val carLabelOffset = 10

  private def drawSnapshot(snapshot: Snapshot) = {
    val boundingBox = snapshot.getBoundingBox

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
      gc.fillOval(car.position.x - carOvalSize / 2, car.position.y - carOvalSize / 2, carOvalSize, carOvalSize)
    }

    gc.fill = Color.Black
    gc.textAlign = TextAlignment.Center
    for (car <- snapshot.cars) {
      gc.fillText(car.id.toString, car.position.x, car.position.y - carLabelOffset)
    }
  }

  private val snapshot = (new RoadGenerator).generate(20, 10)

  private val system = ActorSystem()
  private implicit val ec: ExecutionContext = system.dispatcher

  system.scheduler.schedule(initialDelay = 0 seconds, interval = 1 second) {
    drawSnapshot(snapshot)
  }

}
