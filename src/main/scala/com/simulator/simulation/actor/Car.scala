package com.simulator.simulation.actor

import akka.actor.{Actor, Props}
import akka.event.Logging
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.simulator.Config
import com.simulator.common.CarId
import com.simulator.simulation.actor.Road.EnterRoad
import scalaz.StreamT.Done

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Random

object Car {
  def props(carId: CarId, road: RoadRef): Props =
    Props(new Car(carId, road))

  case object GetState
  final case class GetStateResult(carId: CarId,
                                  road: RoadRef,
                                  positionOnRoad: Double)

  case class EnteredRoad(roadLength: Double, carAhead: Option[CarRef], endJunction: JunctionRef)
  case class CarAheadChanged(carAhead: Option[CarRef])

  case object GetRoadPosition
  case class RoadPosition(road: RoadRef, position: Double)
}

class Car(carId: CarId, initialRoad: RoadRef) extends Actor {

  implicit val ec: ExecutionContext = context.dispatcher
  val log = Logging(context.system, this)
  val random = new Random

  var road: RoadRef = _
  var roadLength: Double = _
  var position: Double = _
  var carAhead: Option[CarRef] = _
  var nextJunction: JunctionRef = _

  override def preStart() {
    log.info("Started")
    initialRoad ! EnterRoad
  }

  def receive = {
    case Car.GetState =>
      sender ! Car.GetStateResult(carId, road, position)

    case Car.EnteredRoad(_roadLength, _carAhead, _nextJunction) =>
      road = sender
      roadLength = _roadLength
      position = 0.0
      carAhead = _carAhead
      nextJunction = _nextJunction

    case Car.CarAheadChanged(_carAhead) =>
      if (sender == road) {
        carAhead = _carAhead
      }

    case Car.GetRoadPosition =>
      sender ! Car.RoadPosition(road, position)

    case TimeSynchronizer.ComputeTimeSlot =>
      assert(0 <= position && position <= roadLength)

      log.info("Computing time slot")
      implicit val timeout: Timeout = 1 second

      val timeSynchronizer = sender

      val futureJunctionState = (nextJunction ? Junction.GetState).mapTo[Junction.State]

      val futureMaybeCarAheadPosition = carAhead
        .map { car =>
          (car ? Car.GetRoadPosition).mapTo[Car.RoadPosition]
            .map { Some(_) }
        }
        .getOrElse(Future.successful(None))
        .map {
          _.collect {
            case Car.RoadPosition(_road, _position) if _road == road => _position
          }
        }

      val futureNextPositionComputed =
        for {
          junctionState <- futureJunctionState
          maybeCarAheadPosition <- futureMaybeCarAheadPosition
        } yield {

          log.info("Car ahead: {}", maybeCarAheadPosition)

          val hasGreenLight = junctionState.roadWithGreenLight.contains(road)
          val junctionOutRoads = junctionState.outRoads

          val increasedPosition = position + Config.carSpeed

          maybeCarAheadPosition match {
            case Some(carAheadPosition) =>
              val allowedPosition = math.max(0.0, carAheadPosition - Config.carDistance)
              assert(allowedPosition <= roadLength)
              position = math.min(increasedPosition, allowedPosition)

            case None =>
              if (increasedPosition >= roadLength && hasGreenLight) {
                val nextRoad = junctionOutRoads(random.nextInt(junctionOutRoads.size))

                position = roadLength
                road ! Road.LeaveRoad
                nextRoad ! Road.EnterRoad

              } else {
                position = math.min(increasedPosition, roadLength)
              }
          }

          Done
        }

      futureNextPositionComputed.map { _ => TimeSynchronizer.TimeSlotComputed }
        .pipeTo(timeSynchronizer)
  }
}
