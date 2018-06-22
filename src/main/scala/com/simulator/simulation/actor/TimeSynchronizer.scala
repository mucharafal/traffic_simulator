package com.simulator.simulation.actor

import akka.NotUsed
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object TimeSynchronizer {
  def props(): Props = Props(new TimeSynchronizer)

  case class AddEntities(entities: Set[ActorRef])
  case class RemoveEntities(entities: Set[ActorRef])
  case object EntitiesChanged

  case object ComputeTimeSlot
  case object TimeSlotComputed
}

class TimeSynchronizer extends Actor {

  implicit val ec: ExecutionContext = context.dispatcher
  val log = Logging(context.system, this)

  var entities = Set.empty[ActorRef]

  var busy = false

  def receive = {
    case TimeSynchronizer.ComputeTimeSlot =>
      log.info("Computing time slot")

      assert(!busy)
      busy = true

      implicit val timeout: Timeout = 1 second

      Future.traverse(entities.toSeq) { _ ? TimeSynchronizer.ComputeTimeSlot }
        .andThen { case _ => busy = false; NotUsed }
        .map { _ => TimeSynchronizer.TimeSlotComputed }
        .pipeTo(sender)

    case TimeSynchronizer.AddEntities(_entities) =>
      entities ++= _entities

    case TimeSynchronizer.RemoveEntities(_entities) =>
      entities --= _entities

  }

}