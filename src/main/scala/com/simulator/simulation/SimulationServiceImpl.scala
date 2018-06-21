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
  private var roadIdToActor: Map[RoadId, RoadRef] = Map.empty
  private var roadActorToId: Map[RoadRef, RoadId] = Map.empty
  private var cars: Seq[(CarId, CarRef)] = Seq.empty

  private def createJunctionActor(junctionState: JunctionState): JunctionRef = {
    val junction = system.actorOf(
      actor.Junction.props(junctionState.id),
      f"junction-${ junctionState.id.value }")

    timeSynchronizer ! actor.TimeSynchronizer.AddInfrastructure(junction)

    junction
  }

  private def createRoadActor(road: RoadState): RoadRef = {
    val endActors = (junctions(road.start), junctions(road.end))

    val (startActor, endActor) = endActors

    val length = Position.distance(
      initialState.junctions.find { _.id == road.start }.get.position,
      initialState.junctions.find { _.id == road.end }.get.position)

    val roadActor = system.actorOf(Road.props(road.id, startActor, endActor, 1.0),
      f"road-${ road.id.value }")

    startActor ! Junction.AddRoad(roadActor, Junction.OutDirection)
    endActor ! Junction.AddRoad(roadActor, Junction.InDirection)
    timeSynchronizer ! TimeSynchronizer.AddInfrastructure(roadActor)

    roadActor
  }

  private def createCarActor(car: CarState): CarRef = {
    val roadActor = roadIdToActor(car.road)

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

    val roads = initialState.roads
      .map { road =>
        road.id -> createRoadActor(road)
      }
    roadIdToActor = roads.toMap
    roadActorToId = roads.map { _.swap }.toMap

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
            val greenLightRoadId = status.roadWithGreenLight.map { roadActorToId(_) }
            initialState.junctions.find { _.id == status.junctionId }.get.copy(greenLightRoad = greenLightRoadId)
          }
      }

      cars: Iterable[CarState] <- Future.traverse(cars) { case (_, car) =>
        ask(car, Car.GetState)
          .mapTo[Car.GetStateResult]
          .map { status =>
            val roadId = roadActorToId(status.road)
            CarState(status.carId, roadId, status.positionOnRoad.toFloat, status.velocity.toFloat, status.breaking)
          }
      }
    } yield {
      Snapshot(junctions.to[Seq], initialState.roads, cars.to[Seq])
    }
  }

}
