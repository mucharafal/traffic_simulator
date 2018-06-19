package com.simulator.ui

import akka.actor.ActorSystem
import com.simulator.roadgenerator.RoadGenerator
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color

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

  private val visualizer = new Visualizer(canvas)

  private val snapshot = (new RoadGenerator).generate(20, 10)

  private val system = ActorSystem()
  private implicit val ec: ExecutionContext = system.dispatcher

  system.scheduler.schedule(initialDelay = 0 seconds, interval = 100 milli) {
    visualizer.drawSnapshot(snapshot)
  }

}
