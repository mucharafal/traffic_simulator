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

  def initialize(): Future[Done] = {
    timeSynchronizer = system.actorOf(actor.TimeSynchronizer.props())

    junctions = initialState.junctions.map { junction =>
      junction.id -> system.actorOf(actor.Junction.props(JunctionTypes.signalizationJunction))
    }.toMap

    roads = initialState.roads.flatMap { road =>
      val startActor = junctions(road.start)
      val endActor = junctions(road.end)

      Seq(
        road.id -> system.actorOf(actor.Road.props(startActor, endActor, 5.0)),
        road.id -> system.actorOf(actor.Road.props(startActor, endActor, 5.0))
      )
    }.toMap

    cars = initialState.cars.map { car =>
      val roadRef = roads(car.road)
      car.id -> system.actorOf(actor.Car.props(car.id, (roadRef, car.positionOnRoad), (roadRef, 1.0), null))
    }.toMap

    Future { Done }
  }

  override def simulateTimeSlot(): Future[Snapshot] = {
    implicit val timeout: Timeout = 1 second

    for {
      junctions <- Future.traverse(junctions) {
        case (id, junction) =>
          ask(junction, actor.Junction.GetStatus)
            .mapTo[actor.Junction.GetStatusResult]
            .map { status => // TODO: use status
              initialState.junctions.find { _.id == id }.get
            }
      }

      cars: Iterable[Car] <- Future.traverse(cars) {
        case (id, car) =>
          ask(car, actor.Car.GetStatus)
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
