package com.simulator

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, Attributes, OverflowStrategy}
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
import scala.concurrent.{Await, ExecutionContext}
import scala.language.postfixOps

object App extends JFXApp {

  private val canvas = new Canvas

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

  private val visualizationService: VisualizationService = new VisualizationServiceImpl(canvas)

  private val roadGenerationService: RoadGenerationService = new RoadGenerationServiceImpl

  private val initialSnapshot = roadGenerationService.generate(10, 20, 50)

  private implicit val system: ActorSystem = ActorSystem()
  private implicit val ec: ExecutionContext = system.dispatcher
  private implicit val materializer: ActorMaterializer = ActorMaterializer()

  private val simulationService: SimulationService = new SimulationServiceImpl(initialSnapshot)

  Await.ready(simulationService.initialize(), 2 second)

  Source.tick(initialDelay = 0 second, interval = 25 milli, NotUsed)
    .mapAsync(parallelism = 1) { _ => simulationService.simulateTimeSlot() }
    .buffer(size = 1, OverflowStrategy.dropHead)
    .async
    .buffer(size = 1, OverflowStrategy.dropHead)
    .withAttributes(Attributes.inputBuffer(1, 1))
    .runForeach { snapshot =>
      visualizationService.visualize(snapshot)
    }
    .failed.foreach { _.printStackTrace() }

  override def stopApp(): Unit = {
    system.terminate()
  }
}
