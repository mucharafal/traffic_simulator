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
    val junctionActor = system.actorOf(
      actor.Junction.props(),
      f"junction-${ junction.id.value }")

    timeSynchronizer ! actor.TimeSynchronizer.AddInfrastructure(junctionActor)

    junctionActor
  }

  private def createRoadActor(road: Road, reversed: Boolean): ActorRef = {
    val endActors = (junctions(road.start), junctions(road.end))

    val (startActor, endActor) = if (reversed) endActors.swap else endActors
    val suffix = if (reversed) "B" else "A"

    val roadActor = system.actorOf(actor.Road.props(road.id, startActor, endActor, 5.0),
      f"road-${ road.id.value }$suffix")

    startActor ! actor.Junction.AddRoad(roadActor, actor.Junction.OutDirection)
    endActor ! actor.Junction.AddRoad(roadActor, actor.Junction.InDirection)
    timeSynchronizer ! actor.TimeSynchronizer.AddCar(roadActor)

    roadActor
  }

  private def createCarActor(car: Car): ActorRef = {
    val roadActor = roads(car.road)

    val carActor = system.actorOf(
      actor.Car.props(car.id, (roadActor, car.positionOnRoad), (roadActor, 1.0), null),
      f"car-${ car.id.value }")

    roadActor ! actor.Road.AddCar(carActor, 0.0, car.positionOnRoad)

    carActor
  }

  override def initialize(): Future[Done] = {
    timeSynchronizer = system.actorOf(actor.TimeSynchronizer.props(), "timeSynchronizer")

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
              val roadId = roads.find { _._2 == status.roadRef }.get._1
              Car(id, roadId, status.positionOnRoad.toFloat, status.velocity.toFloat, status.breaking)
            }
      }
    } yield {
      Snapshot(junctions.to[Seq], initialState.roads, cars.to[Seq])
    }
  }

}
