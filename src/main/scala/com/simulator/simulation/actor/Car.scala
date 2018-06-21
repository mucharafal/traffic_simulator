package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.pattern.ask
import akka.util.Timeout
import com.simulator.common.CarId
import com.simulator.simulation.actor.Road.EnterRoad

import scala.concurrent.duration._
import scala.util.Random

object Car {
  def props(carId: CarId, road: RoadRef, positionOnRoad: Double): Props =
    Props(new Car(carId, road, positionOnRoad))

  case object GetState
  final case class GetStateResult(carId: CarId,
                                  road: RoadRef,
                                  positionOnRoad: Double,
                                  velocity: Double)

  case class EnteredRoad(position: Double, roadLength: Double, carAhead: Option[CarRef], endJunction: JunctionRef)

  case object GetPosition
  case class Position(road: RoadRef, positionOnRoad: Double)
}

class Car(carId: CarId, initialRoad: RoadRef, initialPosition: Double) extends Actor {

  implicit val ec = context.dispatcher
  val log = Logging(context.system, this)
  val random = new Random

  var road: ActorRef = _
  var roadLength: Double = _
  var position: Double = _
  var carAhead: Option[CarRef] = _
  var nextJunction: JunctionRef = _

  override def preStart() {
    log.info("Started")
    initialRoad ! EnterRoad(initialPosition)
  }

  def receive = {
    case Car.GetState =>
      sender ! Car.GetStateResult(carId, road, position, position)

    case Car.EnteredRoad(position, roadLength, carAhead, nextJunction) =>
      this.road = sender
      this.roadLength = roadLength
      this.position = position
      this.carAhead = carAhead
      this.nextJunction = nextJunction

    case TimeSynchronizer.ComputeTimeSlot(_) =>
      log.info("Computing time slot")
      implicit val timeout: Timeout = 500 milli

      val timeSynchronizer = sender

      for {
//        carAheadPosition <- carAhead match {
//          case Some(car) => ask(car, Car.GetPosition).mapTo[Car.Position].map { Some(_) }
//          case None => Future.successful(None)
//        }
        junctionState <- ask(nextJunction, Junction.GetState).mapTo[Junction.GetStateResult]

        increasedPosition = position + 0.05

        (newRoad: RoadRef, newPosition: Double) =
          if (increasedPosition < roadLength) {
            (road, increasedPosition)
          } else if(junctionState.roadWithGreenLight.contains(road)) {
            val outRoads = junctionState.outRoads.toIndexedSeq
            val newRoad = outRoads(random.nextInt(outRoads.size))
            (newRoad, 0.0)
          } else {
            (road, roadLength)
          }
      } yield {
        if (newRoad != road) {
          road ! Road.LeaveRoad
          newRoad ! Road.EnterRoad(position = 0.0)
          position = 0.0
        } else {
          position = newPosition
        }
        log.warning(position.toString)
        timeSynchronizer ! TimeSynchronizer.CarComputed
      }
  }
}
