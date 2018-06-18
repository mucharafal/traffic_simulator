package com.simulator

import javax.sql.RowSetMetaData

import scala.runtime.Nothing$
import akka.actor.{Props, ActorRef, Actor}

object Junction {
  def props(junction: Junction): Props = Props(junction)
  final case class JunctionGetInformationRequest()
  final case class JunctionGetInformationResult(synchronizer: Int, informationPackage: Any)
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

  var synchronizer: Int = -1
  def receive = {
    case JunctionGetInformationRequest() =>
      sender() ! JunctionGetInformationResult(synchronizer, Tuple2(rightHandJunction, RoadsList))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! Computed
  }


}

class SignJunction() extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  var synchronizer: Int = -1
  def receive = {
    case JunctionGetInformationRequest() =>
      sender() ! JunctionGetInformationResult(synchronizer, Tuple2(signJunction, RoadsList))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! Computed
  }
}

class signalizationJunction(val GreenLightTime: Int) extends Junction {
  import JuctionTypes._
  import Junction._
  import TimeSynchronizer._

  var GreenLightRoadRef: ActorRef = null
  var TimeToChange: Int = GreenLightTime
  var synchronizer: Int = -1
  var roadIterator: Int = 0


  def receive = {
    case JunctionGetInformationRequest() =>
      sender() ! JunctionGetInformationResult(synchronizer, Tuple4(signalizationJunction, RoadsList, GreenLightRoadRef, TimeToChange))
    case ComputeTimeSlot(s) =>
      TimeToChange match {
        case 0 =>
          TimeToChange = GreenLightTime
          GreenLightRoadRef = RoadsList.filter(_._2)(roadIterator)._1
          roadIterator = (roadIterator + 1) % RoadsList.size
        case _ =>
          TimeToChange -= 1
      }
      synchronizer = s
      sender() ! Computed
  }

}
