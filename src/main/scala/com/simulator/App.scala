package com.simulator

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

  private val roadGenerationService: RoadGenerationService = new RoadGenerationServiceImpl

  private val initialSnapshot = roadGenerationService.generate(20, 10)

  private val visualizationService: VisualizationService = new VisualizationServiceImpl(canvas)

  private implicit val system = ActorSystem()
  private implicit val ec: ExecutionContext = system.dispatcher

  private val simulationService: SimulationService = new SimulationServiceImpl(initialSnapshot)
  simulationService.initialize()

  system.scheduler.schedule(initialDelay = 0 seconds, interval = 100 milli) {
    val snapshot = simulationService.simulateTimeSlot()
    visualizationService.visualize(snapshot)
  }

}
