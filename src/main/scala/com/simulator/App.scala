package com.simulator

import java.util.concurrent.LinkedBlockingQueue

import akka.NotUsed
import akka.actor.ActorSystem
import com.simulator.roadgeneration.{RoadGenerationService, RoadGenerationServiceImpl}
import com.simulator.simulation.{SimulationService, SimulationServiceImpl}
import com.simulator.visualization.{VisualizationService, VisualizationServiceImpl}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.language.postfixOps

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

  private val roadGenerationService: RoadGenerationService = new RoadGenerationServiceImpl

  private val initialSnapshot = roadGenerationService.generate(10, 20, 50)

  private val visualizationService: VisualizationService = new VisualizationServiceImpl(canvas)

  private implicit val system: ActorSystem = ActorSystem()
  private implicit val ec: ExecutionContext = system.dispatcher

  private val simulationService: SimulationService = new SimulationServiceImpl(initialSnapshot)
  Await.ready(simulationService.initialize(), 2 second)

  val queue = new LinkedBlockingQueue[NotUsed](2)
  system.scheduler.schedule(initialDelay = 1 seconds, interval = 500 milli) {
    queue.offer(NotUsed)
  }

  Future {
    while (true) {
      blocking { queue.take() }
      val snapshot = Await.result(simulationService.simulateTimeSlot(), 2 seconds)
      visualizationService.visualize(snapshot)
    }
  }

  override def stopApp(): Unit = {
    system.terminate()
  }
}
