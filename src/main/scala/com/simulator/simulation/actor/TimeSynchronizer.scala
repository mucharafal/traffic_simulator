package com.simulator.simulation.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

object TimeSynchronizer {
  def props(): Props = Props(new TimeSynchronizer())

  case class AddEntity(entity: ActorRef)
  case class RemoveEntity(entity: ActorRef)

  case object ComputeTimeSlot
  case object TimeSlotComputed
}

class TimeSynchronizer extends Actor {

  implicit val ec: ExecutionContext = context.dispatcher
  val log = Logging(context.system, this)

  var entities = Set.empty[ActorRef]

  def receive = {
    case TimeSynchronizer.ComputeTimeSlot =>
      log.info("Computing time slot")

      implicit val timeout: Timeout = 1 second

      Future.traverse(entities.toSeq) { _ ? TimeSynchronizer.ComputeTimeSlot }
        .map { _ => TimeSynchronizer.TimeSlotComputed }
        .pipeTo(sender)

    case TimeSynchronizer.AddEntity(entity) =>
      entities += entity

    case TimeSynchronizer.RemoveEntity(entity) =>
      entities -= entity

  }

}