package com.simulator.simulation

import akka.Done
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.simulator.common.{BiMap, _}
import com.simulator.simulation.actor._

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class SimulationServiceImpl(initialState: Snapshot)
                           (implicit system: ActorSystem, ec: ExecutionContext) extends SimulationService {

  private implicit val timeout: Timeout = 1 second

  private var timeSynchronizer: TimeSynchronizerRef = _
  private var junctions: BiMap[JunctionId, JunctionRef] = BiMap.empty
  private var roads: BiMap[RoadId, RoadRef] = BiMap.empty
  private var cars: BiMap[CarId, CarRef] = BiMap.empty

  private def createJunctionActor(junctionState: JunctionState): JunctionRef = {
    system.actorOf(
      actor.Junction.props(junctionState.id, greenLightInterval = 5),
      f"junction-${ junctionState.id.value }")
  }

  private def createRoadActor(road: RoadState): RoadRef = {
    val endActors = (junctions(road.start), junctions(road.end))

    val (startActor, endActor) = endActors

    val startPosition = initialState.junctions.find { _.id == road.start }.get.position
    val endPosition = initialState.junctions.find { _.id == road.end }.get.position
    val length = (endPosition - startPosition).length

    val roadActor = system.actorOf(Road.props(road.id, startActor, endActor, length),
      f"road-${ road.id.value }")

    startActor ! Junction.AddRoad(roadActor, Junction.OutDirection)
    endActor ! Junction.AddRoad(roadActor, Junction.InDirection)

    roadActor
  }

  private def createCarActor(car: CarState): CarRef = {
    val roadActor = roads(car.road)

    system.actorOf(
      actor.Car.props(car.id, roadActor),
      f"car-${ car.id.value }")
  }

  override def initialize(): Future[Done] = {
    junctions = initialState.junctions
      .map { junction => junction.id -> createJunctionActor(junction) }
      .toMap[JunctionId, JunctionRef]

    roads = initialState.roads
      .map { road => road.id -> createRoadActor(road) }
      .toMap[RoadId, RoadRef]

    cars = initialState.cars
      .map { car => car.id -> createCarActor(car) }
      .toMap[CarId, CarRef]

    timeSynchronizer = system.actorOf(actor.TimeSynchronizer.props(), "timeSynchronizer")

    val entities = (junctions.values ++ roads.values ++ cars.values).toSet

    (timeSynchronizer ? TimeSynchronizer.AddEntities(entities)).mapTo[Done]
  }

  override def simulateTimeSlot(): Future[Snapshot] = {
    for {
      _ <- ask(timeSynchronizer, TimeSynchronizer.ComputeTimeSlot).mapTo[TimeSynchronizer.TimeSlotComputed.type]

      junctions: Iterable[JunctionState] <- Future.traverse(junctions.values) { junctionActor =>
        ask(junctionActor, actor.Junction.GetState)
          .mapTo[actor.Junction.State]
          .map { status =>
            val greenLightRoadId = status.roadWithGreenLight.map { roads.inverse(_) }
            initialState.junctions.find { _.id == status.junctionId }.get.copy(greenLightRoad = greenLightRoadId)
          }
      }

      cars: Iterable[CarState] <- Future.traverse(cars) { case (_, car) =>
        ask(car, Car.GetState)
          .mapTo[Car.GetStateResult]
          .map { status =>
            val roadId = roads.inverse(status.road)
            CarState(status.carId, roadId, status.positionOnRoad)
          }
      }
    } yield {
      Snapshot(junctions.to[Seq], initialState.roads, cars.to[Seq])
    }
  }

}
