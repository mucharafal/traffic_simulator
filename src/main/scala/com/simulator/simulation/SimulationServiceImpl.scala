package com.simulator.simulation

import akka.Done
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.simulator.common._
import com.simulator.simulation.actor._

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class SimulationServiceImpl(initialState: Snapshot)
                           (implicit system: ActorSystem, ec: ExecutionContext) extends SimulationService {

  private var timeSynchronizer: TimeSynchronizerRef = _
  private var junctions: Map[JunctionId, JunctionRef] = Map.empty
  private var roads: Seq[(RoadId, RoadRef)] = Seq.empty
  private var cars: Seq[(CarId, CarRef)] = Seq.empty

  private def createJunctionActor(junction: JunctionState): JunctionRef = {
    val junctionActor = system.actorOf(
      actor.Junction.props(junction.id),
      f"junction-${ junction.id.value }")

    timeSynchronizer ! actor.TimeSynchronizer.AddInfrastructure(junctionActor)

    junctionActor
  }

  private def createRoadActor(road: RoadState): RoadRef = {
    val (startActor, endActor) = (junctions(road.start), junctions(road.end))

    val length = Position.distance(
      initialState.junctions.find { _.id == road.start }.get.position,
      initialState.junctions.find { _.id == road.end }.get.position)

    val roadActor = system.actorOf(Road.props(road.id, startActor, endActor, 1.0),
      f"road-${ road.id.value }")

    startActor ! actor.Junction.AddRoad(roadActor, actor.Junction.OutDirection)
    endActor ! actor.Junction.AddRoad(roadActor, actor.Junction.InDirection)
    timeSynchronizer ! actor.TimeSynchronizer.AddInfrastructure(roadActor)

    roadActor
  }

  private def createCarActor(car: CarState): CarRef = {
    val roadActor = roads.find { _._1 == car.road }.get._2

    val carActor = system.actorOf(
      actor.Car.props(car.id, roadActor, car.positionOnRoad),
      f"car-${ car.id.value }")

    timeSynchronizer ! actor.TimeSynchronizer.AddCar(carActor)

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
      .map { road =>
        road.id -> createRoadActor(road)
      }

    cars = initialState.cars
      .map { car =>
        (car.id, createCarActor(car))
      }

    Future { Done }
  }

  override def simulateTimeSlot(): Future[Snapshot] = {
    implicit val timeout: Timeout = 1 second

    for {
      _ <- ask(timeSynchronizer, TimeSynchronizer.NextTimeSlot).mapTo[TimeSynchronizer.TimeSlotComputed.type]

      junctions: Iterable[JunctionState] <- Future.traverse(junctions.values) { junctionActor =>
        ask(junctionActor, actor.Junction.GetState)
          .mapTo[actor.Junction.GetStateResult]
          .map { status =>
            val greenLightRoadId = roads.collectFirst { case (roadId, road) if status.roadWithGreenLight.contains(road) => roadId }
            initialState.junctions.find { _.id == status.junctionId }.get.copy(greenLightRoad = greenLightRoadId)
          }
      }

      cars: Iterable[CarState] <- Future.traverse(cars) { case (carId, car) =>
        ask(car, Car.GetState)
          .mapTo[Car.GetStateResult]
          .map { status =>
            val roadId = roads.find { _._2 == status.roadRef }.get._1
            CarState(status.carId, roadId, status.positionOnRoad.toFloat, status.velocity.toFloat, status.breaking)
          }
      }
    } yield {
      Snapshot(junctions.to[Seq], initialState.roads, cars.to[Seq])
    }
  }

}
