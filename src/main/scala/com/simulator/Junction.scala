package com.simulator

import javax.sql.RowSetMetaData

import scala.runtime.Nothing$
import akka.actor.{Props, ActorRef, Actor}

object Junction {
  def props(junction: Junction): Props = Props(junction)
  final case class JunctionGetInformationRequest()
  final case class JunctionGetInformationResult(informationPackage: Any)
}

object JuctionTypes extends Enumeration {
  type JunctionTypes = Value
  val rightHandJunction, signJunction, signalizationJunction = Value
}

abstract class Junction(var RoadsList: List[(ActorRef, Boolean)] = List()) extends Actor {
  def addRoad(roadId: ActorRef, roadInformation: Map[String, Any]) = {
    val begin: Boolean = roadInformation("begin").asInstanceOf[Boolean]
    RoadsList = RoadsList ++ List(Tuple2(roadId, begin))
  }
}

class RightHandJunction() extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  def receive = {
    case JunctionGetInformationRequest() =>
      sender() ! JunctionGetInformationResult(Tuple2(rightHandJunction, RoadsList))
    case ComputeTimeSlot =>
      sender() ! Computed
  }


}

class SignJunction() extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  def receive = {
    case JunctionGetInformationRequest() =>
      sender() ! JunctionGetInformationResult(Tuple2(signJunction, RoadsList))
    case ComputeTimeSlot =>
      sender() ! Computed
  }
}

class signalizationJunction() extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  def receive = {

  }
}
