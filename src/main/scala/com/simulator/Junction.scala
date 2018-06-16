package com.simulator

import javax.sql.RowSetMetaData

import scala.runtime.Nothing$
import akka.actor.{Props, ActorRef, Actor}

object Junction {
  def props(junction: Junction): Props = Props(junction)
  final case class JunctionGetInformationRequest(From: ActorRef)
  final case class JunctionGetInformationResult(From: ActorRef, informationPackage: Any)
}

object JuctionTypes extends Enumeration {
  type JunctionTypes = Value
  val rightHandJunction, signJunction, signalizationJunction = Value
}

abstract class Junction() extends Actor {
  def addRoad(roadId: ActorRef, roadInformation: Any)
}

class RightHandJunction(val JunctionId: ActorRef, var RoadsList: List[(ActorRef, Boolean)]) extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  def receive = {
    case JunctionGetInformationRequest(from) =>
      from ! JunctionGetInformationResult(JunctionId, Tuple2(rightHandJunction, RoadsList))
    case ComputeTimeSlot =>
      2+2     //do nothing
  }

  def addRoad(roadId: ActorRef, roadInformation: Any) = {2}
}

class SignJunction(val JunctionId: ActorRef, var RoadsList: List[(Road, Boolean)]) extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  def receive = {
    case JunctionGetInformationRequest(from) =>
      from ! JunctionGetInformationResult(JunctionId, Tuple2(rightHandJunction, RoadsList))
    case ComputeTimeSlot => 2
  }

  def addRoad(roadId: ActorRef, roadInformation: Any) = {2}
}
