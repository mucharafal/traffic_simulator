package com.simulator

import javax.sql.RowSetMetaData

import scala.runtime.Nothing$
import akka.actor.{Props, ActorRef, Actor}

object JunctionTypes extends Enumeration {
  type JunctionTypes = Value
  val rightHandJunction, signJunction, signalizationJunction = Value
}

object Junction {
  import JunctionTypes._
  def props(junctionType: JunctionTypes): Props = {
    junctionType match {
      case JunctionTypes.rightHandJunction =>
        Props(new RightHandJunction())
      case JunctionTypes.signJunction =>
        Props(new SignJunction())
      case JunctionTypes.signalizationJunction =>
        Props(new SignalizationJunction())
    }
  }
  final case class JunctionGetInformationResult(synchronizer: Int, informationPackage: Any)
  final case class AddRoad(id: ActorRef, roadInformation: Map[String, Any])
  case object JunctionGetInformationRequest
}

abstract class Junction(var RoadsList: List[(ActorRef, Boolean)] = List()) extends Actor {
  def addRoad(roadId: ActorRef, roadInformation: Map[String, Any]) = {
    print("Adding road\n")
    val begin: Boolean = roadInformation("begin").asInstanceOf[Boolean]
    RoadsList = RoadsList ++ List(Tuple2(roadId, begin))
  }
}

class RightHandJunction() extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._

  var synchronizer: Int = -1
  def receive = {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, Tuple2(rightHandJunction, RoadsList))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! Computed
    case AddRoad(id, map) =>
      super.addRoad(id, map)
  }


}
object SignJunction {
  final case class priviledgeRoad(road: ActorRef)
}
class SignJunction() extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._
  import SignJunction._

  var synchronizer: Int = -1
  var privilegedRoads: (ActorRef, ActorRef) = Tuple2(null, null)
  def receive = {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, Tuple3(signJunction, RoadsList, privilegedRoads))
    case ComputeTimeSlot(s) =>
      synchronizer = s
      sender() ! Computed
    case AddRoad(id, map) =>
      super.addRoad(id, map)
    case priviledgeRoad(ref) =>
      privilegedRoads = privilegedRoads match {
        case (null, null) =>
          Tuple2(ref, null)
        case (sth, null) =>
          Tuple2(sth, ref)
        case (sth, sth2) =>
          privilegedRoads
      }
  }
}

class SignalizationJunction(val GreenLightTime: Int = 10) extends Junction {
  import JunctionTypes._
  import Junction._
  import TimeSynchronizer._

  var GreenLightRoadRef: ActorRef = null
  var TimeToChange: Int = GreenLightTime
  var synchronizer: Int = -1
  var roadIterator: Int = 0


  def receive = {
    case JunctionGetInformationRequest =>
      sender() ! JunctionGetInformationResult(synchronizer, Tuple4(signalizationJunction, RoadsList, GreenLightRoadRef, TimeToChange))
    case ComputeTimeSlot(s) =>
      TimeToChange match {
        case 0 =>
          TimeToChange = GreenLightTime
          GreenLightRoadRef = RoadsList.filter(_._2 == false)(roadIterator)._1
          roadIterator = (roadIterator + 1) % RoadsList.size
        case _ =>
          TimeToChange -= 1
      }
      synchronizer = s
      sender() ! Computed
    case AddRoad(id, map) =>
      super.addRoad(id, map)
  }
}
