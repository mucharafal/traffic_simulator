package com.simulator.simulation

import akka.Done
import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.simulator.common._
import com.simulator.simulation.actor.JunctionTypes

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class SimulationServiceImpl(initialState: Snapshot)
                           (implicit system: ActorSystem, ec: ExecutionContext) extends SimulationService {

  private var timeSynchronizer: ActorRef = _
  private var junctions: Map[JunctionId, ActorRef] = Map.empty
  private var roads: Map[RoadId, ActorRef] = Map.empty
  private var cars: Map[CarId, ActorRef] = Map.empty

  private def createJunctionActor(junction: Junction): ActorRef = {
    system.actorOf(
      actor.Junction.props(JunctionTypes.signalizationJunction),
      f"junction-${ junction.id.value }")
  }

  private def createRoadActor(road: Road, reversed: Boolean): ActorRef = {
    val endActors = (junctions(road.start), junctions(road.end))

    val (startActor, endActor) = if (reversed) endActors.swap else endActors
    val suffix = if (reversed) "B" else "A"

    system.actorOf(actor.Road.props(startActor, endActor, 5.0),
      f"road-${ road.id.value }$suffix")
  }

  private def createCarActor(car: Car): ActorRef = {
    val roadRef = roads(car.road)

    system.actorOf(
      actor.Car.props(car.id, (roadRef, car.positionOnRoad), (roadRef, 1.0), null),
      f"car-${ car.id.value }")
  }

  override def initialize(): Future[Done] = {
    timeSynchronizer = system.actorOf(actor.TimeSynchronizer.props())

    junctions = initialState.junctions
      .map { junction =>
        junction.id -> createJunctionActor(junction)
      }
      .toMap

    roads = initialState.roads
      .flatMap { road =>
        Seq(
          road.id -> createRoadActor(road, reversed = false),
          road.id -> createRoadActor(road, reversed = true)
        )
      }
      .toMap

    cars = initialState.cars
      .map { car =>
        car.id -> createCarActor(car)
      }
      .toMap

    Future { Done }
  }

  override def simulateTimeSlot(): Future[Snapshot] = {
    implicit val timeout: Timeout = 1 second

    for {
      junctions: Iterable[Junction] <- Future.traverse(junctions) {
        case (id, junctionActor) =>
          ask(junctionActor, actor.Junction.GetStatus)
            .mapTo[actor.Junction.GetStatusResult]
            .map { status => // TODO: use status
              initialState.junctions.find { _.id == id }.get
            }
      }

      cars: Iterable[Car] <- Future.traverse(cars) {
        case (id, carActor) =>
          ask(carActor, actor.Car.GetStatus)
            .mapTo[actor.Car.GetStatusResult]
            .map { status =>
              val roadId = roads.find { _._2 == status.roadId }.get._1
              Car(id, roadId, status.position_x.toFloat, status.velocity.toFloat, status.breaking)
            }
      }
    } yield {
      Snapshot(junctions.to[Seq], initialState.roads, cars.to[Seq])
    }
  }

}
